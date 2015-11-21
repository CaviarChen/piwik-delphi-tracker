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
    procedure doTrackUserInfo();
    procedure doTrackUrl(_url:string);
    procedure doTrackEvent(category,action,name:string;value:double);
    procedure doTrackContent(name,piece,target,interaction:string);
  end;

implementation

uses FMX.Forms, DateUtils;

procedure TPiwikTracker.MainLoop();
begin
  Thread :=TThread.CreateAnonymousThread(
  procedure
  var url,fullurl:string;
      retry:integer;
  begin
    while True do
    begin
      if retry > 5 then begin retry := 3;Event.ResetEvent;Event.WaitFor(2*60*1000);end
        else Event.WaitFor();
      if Thread.CheckTerminated then exit;

      url := '';
      TThread.Synchronize(TThread.Current,
      procedure
      begin
        if SendQueue.Count<>0 then url := SendQueue.Dequeue;¡¡
      end);

      if url='' then begin Event.ResetEvent; Continue; end;

      fullurl := basic_request_url+'&'+url+'&rand='+IntToStr(Random(100000));

      try
        idhttp.Get(fullurl);
        retry := 0;
      except
        retry := retry+1;
        TThread.Synchronize(TThread.Current,
        procedure
        begin
          SendQueue.Enqueue(url);
        end);
      end;

      Event.SetEvent;

    end;
  end
  );
  Thread.Start;
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
  Thread.WaitFor;
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
  url := '_cvar={';
  for i := 0 to CustomVariable.Count-1 do
    begin
      if i<>0 then url := url+',';
      url := url + Format('"%d":["%s","%s"]',[i+1,CustomVariable.Names[i],CustomVariable.ValueFromIndex[i]]);
    end;
  url := url+'}';

  SendQueue.Enqueue(url);
  Event.SetEvent;
end;

procedure TPiwikTracker.doTrackUserInfo();
var url:string;
begin
  url := Format('res=%d¡Á%d',[Screen.Size.cx,Screen.Size.cy]); //Screen Resolution
  url := url + Format('&h=%d&m=%d&s=%d',[HourOf(now),MinuteOf(now),SecondOf(now)]); //Time

  SendQueue.Enqueue(url);
  Event.SetEvent;
end;

procedure TPiwikTracker.doTrackUrl(_url:string);
var url:string;
begin
  url := 'url='+_url;

  SendQueue.Enqueue(url);
  Event.SetEvent;
end;

procedure TPiwikTracker.doTrackEvent(category,action,name:string;value:double);
var url:string;
begin
  url := Format('e_c=%s&e_a=%s',[category,action]);
  if name<>'' then url := url+'&e_n='+name;
  if value<>0 then url := url+'&e_v='+FloatToStr(value);

  SendQueue.Enqueue(url);
  Event.SetEvent;
end;

procedure TPiwikTracker.doTrackContent(name,piece,target,interaction:string);
var url:string;
begin
  url := '&c_n='+name;
  if piece<>'' then url := url+'&c_p='+piece;
  if target<>'' then url := url+'&c_t='+target;
  if interaction<>'' then url := url+'&c_i='+interaction;

  SendQueue.Enqueue(url);
  Event.SetEvent;
end;
end.
