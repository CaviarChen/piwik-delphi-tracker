unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, piwik,
  FMX.Controls.Presentation, FMX.StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, FMX.WebBrowser;

type
  TForm1 = class(TForm)
    CornerButton1: TCornerButton;
    CornerButton2: TCornerButton;
    Button1: TButton;
    Button2: TButton;
    procedure CornerButton1Click(Sender: TObject);
    procedure CornerButton2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

var tracker:TPiwikTracker;

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
begin
//  ShowMessage(inttostr(TOSVersion.ServicePackMajor));
//  ShowMessage(IntToStr(TOSVersion.Major));
//  ShowMessage(IntToStr(TOSVersion.Minor));
end;

procedure TForm1.CornerButton1Click(Sender: TObject);
var UA:string;
begin
  tracker:= TPiwikTracker.Create('http://vs4.vlabpro.com/piwik/piwik.php',1,Piwik_GenerateCID,'');



  UA := 'Mozilla/3.0 '+Piwik_GenerateSystemOfUA;


  tracker.SetUserAgent(UA);
//  tracker.Free;
end;

procedure TForm1.CornerButton2Click(Sender: TObject);
begin
  tracker.doTrackUserInfo;
  tracker.SetCustomVariable('test','A');
  tracker.doTrackCustomVariable;

end;

end.
