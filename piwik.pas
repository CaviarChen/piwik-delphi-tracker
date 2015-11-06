unit piwik;

interface
uses
  System.SysUtils, System.Classes, System.SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;


type
  TPiwikTracker = class(TObject)
  private
  var 
    basic_request_url:string;
    idhttp:TidHttp;
    Event:TEvent;

  public
    constructor Create(Piwik_url:string;idsite:integer);
    destructor Destroy; override;

    procedure SetUserAgent(UA:string);
    procedure SetTimeout(t:integer);

  end;

implementation

constructor TPiwikTracker.Create(Piwik_url:string;idsite:integer);
begin
  basic_request_url := Piwik_url + '?rec=1&idsite=' + IntToStr(idsite);
  idhttp := TIdHTTP.Create();
  idhttp.HandleRedirects := True;
  Event := TEvent.Create();
end;

destructor TPiwikTracker.Destroy;
begin
  idhttp.Free;
  Event.Free;
end;

procedure TPiwikTracker.SetUserAgent(UA:string);
begin
  idhttp.Request.UserAgent := UA;
end;

procedure TPiwikTracker.SetTimeout(t:integer);
begin
  idhttp.ReadTimeout := t;
end;


end.
