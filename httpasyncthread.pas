unit HTTPAsyncThread;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fphttpclient, HTTPDefs, DateUtils;

type
    TSyncRequestParamsEvent = procedure(var request: string; var user, pass: string) of object;
    TSyncResponseDataEvent = procedure(response: string; cookies: TStrings) of object;

    THTTPAsyncThread = class(TThread)
    private
        fRequest: string;
        fAnswer: string;
        fUser, fPass: string;
        fCookieJar: TStrings;
        getNewRequest: boolean;
        HTTPClient: TFPHttpClient;
        FSynchRequestParams: TSyncRequestParamsEvent;
        FSynchResponseData: TSyncResponseDataEvent;
        procedure DoSyncRequestParams;
        procedure DoSyncResponseData;

    protected
        procedure Execute; override;
    public
        constructor Create(CreateSuspended: boolean);
        class procedure CreateOrRecycle(var instanceVar: THTTPAsyncThread);
        property CookieJar: TStrings read fCookieJar write fCookieJar;
        property OnSyncRequestParams: TSyncRequestParamsEvent read FSynchRequestParams write FSynchRequestParams;
        property OnSynchResponseData: TSyncResponseDataEvent read FSynchResponseData write FSynchResponseData;
    end;


implementation

class procedure THTTPAsyncThread.CreateOrRecycle(var instanceVar: THTTPAsyncThread);
begin
    if (instanceVar = nil) then
        instanceVar := THTTPAsyncThread.Create(False)
    else
        if (instanceVar.Finished) then
        begin
            instanceVar.Free;
            instanceVar := THTTPAsyncThread.Create(False);
        end
        else
            instanceVar.getNewRequest := True;
end;

constructor THTTPAsyncThread.Create(CreateSuspended: boolean);
begin
    inherited Create(CreateSuspended);
    FreeOnTerminate := False;
    getNewRequest := True;
end;

procedure THTTPAsyncThread.DoSyncRequestParams;
begin
    if Assigned(FSynchRequestParams) then
        FSynchRequestParams(fRequest, fUser, fPass);
    getNewRequest := False;
end;

procedure THTTPAsyncThread.DoSyncResponseData;
begin
    if Assigned(FSynchResponseData) then
        FSynchResponseData(fAnswer, fCookieJar);
end;

procedure THTTPAsyncThread.Execute;
//var
begin
    // get Request parameters
    while (not Terminated) do
    begin
        if (getNewRequest) then
            // FIXME: next line produces call trace
            Synchronize(@DoSyncRequestParams);
        // make http call
        HttpClient := TFPHttpClient.Create(nil);
        try
            HttpClient.AllowRedirect := True;
            HttpClient.UserName := fUser;
            HttpClient.Password := fPass;
            HttpClient.AddHeader('Content-Type', 'application/json');
            HttpClient.AddHeader('Accept', 'application/json');
            // FIXME: add X-Auth-Namespace header
            HttpClient.AddHeader('X-Auth-Namespace', 'humanusername');

            fAnswer := HttpClient.Get(fRequest);
            fCookieJar:= TStringList.Create;
            fCookieJar.Text:= HttpClient.Cookies.Text;
        finally
            HttpClient.Free;
        end;
        // copy back result
        Synchronize(@DoSyncResponseData);
        Terminate;
    end;
end;

end.

