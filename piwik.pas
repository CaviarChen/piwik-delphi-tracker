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
    c_url,c_urlref:string;
    Thread:TThread;

    procedure MainLoop();
    procedure SubmitUrl(_url:string);

  public
  var
    SendQueue:TQueue<string>;

    constructor Create(Piwik_url:string;idsite:integer;cid,uid:string);
    destructor Destroy; override;

    procedure SetUserAgent(UA:string);
    procedure SetTimeout(t:integer);
    procedure SetCustomVariable(_name,_value:string);
    procedure SetCurrentURL(_url,urlref:string);

    //------------------------------------

    procedure doTrackCustomVariable();
    procedure doTrackUserInfo();
    procedure doTrackEvent(category,action,name:string;value:double);
    procedure doTrackContent(name,piece,target,interaction:string);
    procedure doTrackAction(Action_name:string);
  end;

function Piwik_GenerateSystemOfUA():string;
function Piwik_GenerateCID():string;

implementation

uses FMX.Forms, DateUtils, IdURI;

function Piwik_GenerateSystemOfUA():string;
var UA:string;
begin
  if TOSVersion.Platform=TOSVersion.TPlatform.pfWindows then
    begin
      UA := UA+'('+Format('Windows NT %d.%d',[TOSVersion.Major,TOSVersion.Minor]);
      if TOSVersion.Architecture=TOSVersion.TArchitecture.arIntelX64 then UA := UA+';WOW64';
      UA := UA+')';
    end;

  if TOSVersion.Platform=TOSVersion.TPlatform.pfMacOS then
    begin
      UA := UA+Format('(Macintosh; Intel Mac OS X %d_%d_%d)',[TOSVersion.Major,TOSVersion.Minor,TOSVersion.ServicePackMajor]);
    end;

  Result := UA;
end;

function Piwik_GenerateCID():string;
const
  ch='01234567890abcdef';
var
  i:integer;
begin
  Randomize;
  Result := '';
  for i := 1 to 16 do
    Result := Result+ch[Random(Length(ch)) + 1];
end;

//--------------------------------------------------------------

constructor TPiwikTracker.Create(Piwik_url:string;idsite:integer;cid,uid:string);
begin
  basic_request_url := Piwik_url + '?rec=1&idsite=' + IntToStr(idsite) + '&cid=' + cid;
  if uid<>'' then basic_request_url := basic_request_url + '&uid=' + uid;
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

procedure TPiwikTracker.SetCurrentURL(_url,urlref:string);
begin
  c_url := _url;
  c_urlref := urlref;
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
      url := url + Format('"%d":["%s","%s"]',[i+1,TIdURI.URLEncode(CustomVariable.Names[i]),TIdURI.URLEncode(CustomVariable.ValueFromIndex[i])]);
    end;
  url := url+'}';

  Self.SubmitUrl(url);
end;

procedure TPiwikTracker.doTrackUserInfo();
var url:string;
begin
  url := Format('res=%dx%d',[Screen.Size.cx,Screen.Size.cy]); //Screen Resolution
  url := url + Format('&h=%d&m=%d&s=%d',[HourOf(now),MinuteOf(now),SecondOf(now)]); //Time

  Self.SubmitUrl(url);
end;

procedure TPiwikTracker.doTrackEvent(category,action,name:string;value:double);
var url:string;
begin
  url := Format('e_c=%s&e_a=%s',[TIdURI.URLEncode(category),TIdURI.URLEncode(action)]);
  if name<>'' then url := url+'&e_n='+TIdURI.URLEncode(name);
  if value<>0 then url := url+'&e_v='+FloatToStr(value);

  Self.SubmitUrl(url);
end;

procedure TPiwikTracker.doTrackContent(name,piece,target,interaction:string);
var url:string;
begin
  url := '&c_n='+TIdURI.URLEncode(name);
  if piece<>'' then url := url+'&c_p='+TIdURI.URLEncode(piece);
  if target<>'' then url := url+'&c_t='+TIdURI.URLEncode(target);
  if interaction<>'' then url := url+'&c_i='+TIdURI.URLEncode(interaction);

  Self.SubmitUrl(url);
end;

procedure TPiwikTracker.doTrackAction(Action_name:string);
var url:string;
begin
  url := '&action_name='+TIdURI.URLEncode(Action_name);

  Self.SubmitUrl(url);
end;

procedure TPiwikTracker.SubmitUrl(_url:string);
begin
  if c_url='' then _url := _url + '&url=' + TIdURI.URLEncode(c_url);
  if c_urlref='' then _url := _url + '&urlref=' + TIdURI.URLEncode(c_urlref);

  SendQueue.Enqueue(_url);
  Event.SetEvent;
end;

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
      Randomize;
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

end.
