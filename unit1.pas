unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, httpasyncthread, HTTPDefs, DateUtils, IniFiles;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure CopyRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
    procedure CopyResponse(response: httpResponse);
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
    requestOverdue: integer;
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


procedure TForm1.CopyRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
begin
    // get request params from main thread
    user:= _user;
    pass:= _pass;
    request:= _request;
    authnamespace:= _authnamespace;

end;

procedure TForm1.CopyResponse(response: httpResponse);
begin
    // pass data from http thread back to main thread
    Memo1.Text:= response.text;
    Memo2.Text:= IntToStr(response.status);
    requestReceived:= True;
    CheckResponse(self);
end;

procedure TForm1.CheckResponse(Sender: TObject);
var
  secondsOut: integer;
  numDots: string;
  c: integer;

begin
    if (requestReceived) then
    begin
      StatusBar1.SimpleText:= '';
      Timer1.Enabled:= False;
    end else
    begin
      requestOverdue:= MilliSecondsBetween(timeAtLastRequest, Now);
      if (requestOverdue > waitFor) then
      begin
        secondsOut:= Round(requestOverdue / 1000);
        for c:=1 to secondsOut do
          numDots:= numDots + '.';
        StatusBar1.SimpleText:= 'Waiting for server response' + numDots;
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

