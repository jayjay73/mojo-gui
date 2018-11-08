unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, httpasyncthread, HTTPDefs, DateUtils;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure CopyRequest(var request: string; var user, pass: string);
    procedure CopyResponse(response: string; cookies: TStrings);

    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    _request: string;
    _response: string;

  public

  end;

var
  Form1: TForm1;
  myHTTPAsyncThread: THTTPAsyncThread = nil;

implementation

{$R *.lfm}

{ TForm1 }

//*******************************


procedure TForm1.CopyRequest(var request: string; var user, pass: string);
begin
    // get request params from main thread
    user:= 'tokenname';
    pass:= 'tokenpass';
    request:= 'https://myhost.tld:3443/project-name/API/v1.0/endpoint1';
end;

procedure TForm1.CopyResponse(response: string; cookies: TStrings);
begin
    // pass data to main thread
    Memo1.Text:= response;
end;



//*******************************

procedure TForm1.Button1Click(Sender: TObject);
begin
    if (assigned(myHTTPAsyncThread)) then
       Memo2.Text:= myHTTPAsyncThread.ToString;

    myHTTPAsyncThread.makeHTTPRequest;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    THTTPAsyncThread.CreateOrRecycle(myHTTPAsyncThread);
    myHTTPAsyncThread.OnSyncRequestParams:= @CopyRequest;
    myHTTPAsyncThread.OnSynchResponseData:= @CopyResponse;
end;


end.

