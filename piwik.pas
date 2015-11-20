unit piwik;

interface
uses
  System.SysUtils, System.Classes, System.SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Generics.Collections, FMX.dialogs;


type
  TPiwikTracker = class(TObject)
  private
  var 
    basic_request_url:string;
    idhttp:TidHttp;
    Event:TEvent;
    CustomVariable:TStringList;
    SendQueue:TQueue<string>;
    Thread:TThread;
    procedure MainLoop();

  public
    constructor Create(Piwik_url:string;idsite:integer);
    destructor Destroy; override;

    procedure SetUserAgent(UA:string);
    procedure SetTimeout(t:integer);
    procedure SetCustomVariable(_name,_value:string);

    //------------------------------------

    procedure doTrackCustomVariable();

  end;

implementation

procedure TPiwikTracker.MainLoop();
begin
  Thread :=TThread.CreateAnonymousThread(
  procedure
  begin
    while True do begin
    begin
      Event.WaitFor();
      if Terminated then exit;

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        showmessage('X');
      end);
    end;
  end
  ).Start;
end;



constructor TPiwikTracker.Create(Piwik_url:string;idsite:integer);
begin
  basic_request_url := Piwik_url + '?rec=1&idsite=' + IntToStr(idsite);
  idhttp := TIdHTTP.Create();
  idhttp.HandleRedirects := True;
  Event := TEvent.Create();
  SendQueue := TQueue<string>.Create;
  CustomVariable := TStringList.Create;

  Self.MainLoop;
end;

destructor TPiwikTracker.Destroy;
begin
  Thread.Terminate;
  Event.SetEvent;

  idhttp.Free;
  Event.Free;
  SendQueue.Free;
  CustomVariable.Free;
end;

procedure TPiwikTracker.SetUserAgent(UA:string);
begin
  idhttp.Request.UserAgent := UA;
end;

procedure TPiwikTracker.SetTimeout(t:integer);
begin
  idhttp.ReadTimeout := t;
end;

procedure TPiwikTracker.SetCustomVariable(_name,_value:string);
begin
  if _value=''
  then
    CustomVariable.Delete(CustomVariable.IndexOfName(_name))
  else
    CustomVariable.Values[_name] := _value;
end;


//-----------------------

procedure TPiwikTracker.doTrackCustomVariable();
var url:string;
    i:integer;
begin
  //_cvar={"1":["OS","iphone 5.0"],"2":["Piwik Mobile Version","1.6.2"],"3":["Locale","en::en"],"4":["Num Accounts","2"]}
//  url := '_cvar={'
//
//  Event.SetEvent;
end;

end.
