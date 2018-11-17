unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, httpasyncthread, HTTPDefs, DateUtils, IniFiles,
  fpjson, jsonparser;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure Button2Click(Sender: TObject);
    procedure CopyRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
    procedure CopyResponse(response: httpResponse);
    procedure CheckResponse(Sender: TObject);

    procedure CopyTokenRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
    procedure CopyTokenResponse(response: httpResponse);

    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    _username: string;
    _adpass: string;
    _user: string;
    _pass: string;
    _request: string;
    _baseurl: string;
    _testendpoint: string;
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
  tokenHTTPAsyncThread: THTTPAsyncThread = nil;
  INI: TINIFile;

implementation

{$R *.lfm}

{ TForm1 }

//*******************************

procedure TForm1.Button2Click(Sender: TObject);
begin
    timeAtLastRequest:= Now;
    requestReceived:= False;
    StatusBar1.SimpleText:= '';
    waitFor:= 1000;
    Timer1.Enabled:= True;
    Timer1.OnTimer:= @CheckResponse;
    tokenHTTPAsyncThread.makeHTTPRequest;
end;

procedure TForm1.CopyRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
begin
    // get request params from main thread
    user:= _user;
    pass:= _pass;
    //request:= _baseurl + '/exasol-sandbox/API/v1.0/users';
    request:= _baseurl + '/' + _testendpoint;
    authnamespace:= _authnamespace;

end;

procedure TForm1.CopyResponse(response: httpResponse);
begin
    // pass data from http thread back to main thread
    Memo1.Text:= response.text;
    Memo2.Text:= IntToStr(response.status);
    requestReceived:= True;
    StatusBar1.SimpleText:= '';
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
      //StatusBar1.SimpleText:= '';
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

procedure TForm1.CopyTokenRequest(var request: string; var user: string; var pass: string; var authnamespace: string);
begin
    // get request params from main thread
    user:= _username;
    pass:= _adpass;
    authnamespace:= _authnamespace;
    request:= _baseurl + '/tokens';
end;

procedure TForm1.CopyTokenResponse(response: httpResponse);
begin
    // pass data from http thread back to main thread
    if (response.status = 200) then
    begin
       ListBox1.Items.Add(response.text);
       StatusBar1.SimpleText:= '';
    end
    else
       StatusBar1.SimpleText:= response.text;
    Memo2.Text:= IntToStr(response.status);
    requestReceived:= True;
    CheckResponse(self);
end;

//*******************************

procedure TForm1.Button1Click(Sender: TObject);
begin
    if (assigned(myHTTPAsyncThread)) then
       Memo2.Text:= myHTTPAsyncThread.ToString;
    timeAtLastRequest:= Now;
    requestReceived:= False;
    StatusBar1.SimpleText:= '';
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
        _baseurl:= INI.ReadString('MojoTest','BASEURL','');
        _testendpoint:= INI.ReadString('MojoTest','TESTENDPOINT','');
        _username:= INI.ReadString('User','USERNAME','');
        _adpass:= INI.ReadString('User','ADPASS','');
        Memo2.Text:= _baseurl;
    finally
      INI.Free;
    end;
    THTTPAsyncThread.CreateOrRecycle(myHTTPAsyncThread);
    myHTTPAsyncThread.OnSyncRequestParams:= @CopyRequest;
    myHTTPAsyncThread.OnSynchResponseData:= @CopyResponse;
    Timer1.Enabled:= False;

    THTTPAsyncThread.CreateOrRecycle(tokenHTTPAsyncThread);
    tokenHTTPAsyncThread.OnSyncRequestParams:= @CopyTokenRequest;
    tokenHTTPAsyncThread.OnSynchResponseData:= @CopyTokenResponse;

end;


end.

