unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, httpasyncthread, HTTPDefs, DateUtils, IniFiles;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure CopyRequest(var request: string; var user, pass: string);
    procedure CopyResponse(response: string; cookies: TStrings);
    procedure CheckResponse(Sender: TObject);

    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    _user: string;
    _pass: string;
    _request: string;
    _response: string;
    _authNamespace: string;
    requestReceived: boolean;
    timeAtLastRequest: TDateTime;
    waitFor: integer;

  public

  end;

var
  Form1: TForm1;
  myHTTPAsyncThread: THTTPAsyncThread = nil;
  INI: TINIFile;

implementation

{$R *.lfm}

{ TForm1 }

//*******************************


procedure TForm1.CopyRequest(var request: string; var user, pass: string);
begin
    // get request params from main thread
    user:= _user;
    pass:= _pass;
    request:= _request;

end;

procedure TForm1.CopyResponse(response: string; cookies: TStrings);
begin
    // pass data from http thread back to main thread
    Memo1.Text:= response;
    requestReceived:= True;
end;

procedure TForm1.CheckResponse(Sender: TObject);
begin
    if (requestReceived) then
    begin
      StatusBar1.SimpleText:= '';
      Timer1.Enabled:= False;
    end else
    begin
      if (MilliSecondsBetween(timeAtLastRequest, Now) > waitFor) then
      begin
        StatusBar1.SimpleText:= 'Waiting for server response...';
        Timer1.Enabled:= True;
      end;
    end;
end;

//*******************************

procedure TForm1.Button1Click(Sender: TObject);
begin
    if (assigned(myHTTPAsyncThread)) then
       Memo2.Text:= myHTTPAsyncThread.ToString;
    timeAtLastRequest:= Now;
    requestReceived:= False;
    waitFor:= 1000;
    Timer1.Enabled:= True;
    Timer1.OnTimer:= @CheckResponse;
    myHTTPAsyncThread.makeHTTPRequest;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    INI := TINIFile.Create('mojo-test.ini');
    try
        _user:= INI.ReadString('MojoTest','USER','');
        _pass:= INI.ReadString('MojoTest','PASS','');
        _authNamespace:= INI.ReadString('MojoTest','AUTHNAMESPACE','');
        _request:= INI.ReadString('MojoTest','REQUEST','');
        Memo2.Text:= _request;
    finally
      INI.Free;
    end;
    THTTPAsyncThread.CreateOrRecycle(myHTTPAsyncThread);
    myHTTPAsyncThread.OnSyncRequestParams:= @CopyRequest;
    myHTTPAsyncThread.OnSynchResponseData:= @CopyResponse;
    Timer1.Enabled:= False;
end;


end.

