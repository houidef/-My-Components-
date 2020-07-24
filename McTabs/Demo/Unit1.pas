unit Unit1;

interface                 

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Tabs, mcTabs, XPMan, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    MCTabSet1: TMCTabSet;
    XPManifest1: TXPManifest;
    lbHomePage: TLabel;
    Label4: TLabel;
    lbGetSrc: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure lbHomePageMouseEnter(Sender: TObject);
    procedure lbHomePageMouseLeave(Sender: TObject);
    procedure lbHomePageClick(Sender: TObject);
    procedure lbGetSrcClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.lbHomePageMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Cursor := crHandPoint;
  TLabel(Sender).Font.Color := clPurple;//clHighLight;
  TLabel(Sender).Font.Style := TLabel(Sender).Font.Style + [fsUnderline];
end;

procedure TForm1.lbHomePageMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Cursor := crDefault;
  TLabel(Sender).Font.Color := clHighLight;//clHotLight;
  TLabel(Sender).Font.Style := TLabel(Sender).Font.Style - [fsUnderline];
end;

procedure TForm1.lbHomePageClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, 'http://www.MasterCluster.com/', nil, nil, 0);
end;

procedure TForm1.lbGetSrcClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, 'http://shareit.com/register.html?productid=191538', nil, nil, 0);
end;

end.
