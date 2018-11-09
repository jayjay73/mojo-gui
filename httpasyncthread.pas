unit HTTPAsyncThread;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fphttpclient, HTTPDefs, DateUtils, syncobjs;

type
    TSyncRequestParamsEvent = procedure(var request: string; var user, pass: string) of object;
    TSyncResponseDataEvent = procedure(response: string; cookies: TStrings) of object;

    THTTPAsyncThread = class(TThread)
        procedure makeHTTPRequest;
        procedure makeHTTPRequest(request: string; user: string; pass: string);
    private
        semaphore: TEventObject;
        fRequest: string;
        fAnswer: string;
        fUser, fPass: string;
        fCookieJar: TStrings;
        getRequestFromCallback: boolean;
        workToDo: boolean;
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
        end;
        //else
            //instanceVar.getRequestFromCallback := True;
            //workToDo:= True;
            // do nothing for now
end;

constructor THTTPAsyncThread.Create(CreateSuspended: boolean);
begin
    inherited Create(CreateSuspended);
    FreeOnTerminate := False;
    getRequestFromCallback := True;
    workToDo:= False;
    semaphore:= TEventObject.Create(nil,true,false,'');
end;

procedure THTTPAsyncThread.Execute;
//var
begin
    // get Request parameters
    while (not Terminated) do
    begin
        semaphore.WaitFor(INFINITE);
        semaphore.ResetEvent;
        if (workToDo) then
        begin
            if (getRequestFromCallback) then
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
                HttpClient.AddHeader('X-Auth-Namespace', 'jaegerj');

                fAnswer := HttpClient.Get(fRequest);
                fCookieJar:= TStringList.Create;
                fCookieJar.Text:= HttpClient.Cookies.Text;
            finally
                HttpClient.Free;
            end;
            // copy back result
            Synchronize(@DoSyncResponseData);
        end;
        //Terminate;
        // don't terminate anymore since we keep the thread around
    end;
    // terminate by falling off the edge
end;

procedure THTTPAsyncThread.DoSyncRequestParams;
begin
    if Assigned(FSynchRequestParams) then
        FSynchRequestParams(fRequest, fUser, fPass);
    getRequestFromCallback := False;
end;

procedure THTTPAsyncThread.DoSyncResponseData;
begin
    if Assigned(FSynchResponseData) then
        FSynchResponseData(fAnswer, fCookieJar);
end;

procedure THTTPAsyncThread.makeHTTPRequest;
begin
    getRequestFromCallback:= True;
    workToDo:= True;
    semaphore.SetEvent;
end;

procedure THTTPAsyncThread.makeHTTPRequest(request: string; user: string; pass: string);
begin
    getRequestFromCallback:= False;
    fRequest:= request;
    if (user <> '') then fUser:= user;
    if (pass <> '') then fPass:= pass;
    workToDo:= True;
    semaphore.SetEvent;
end;

end.

