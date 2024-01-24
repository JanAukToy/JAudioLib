unit Form_Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.ComCtrls, Vcl.Controls,
  Vcl.ToolWin, Vcl.Menus, System.Generics.Collections, Vcl.StdCtrls,

  cls_AudioStreamClientThread, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    pnl_Settings: TPanel;
    pnl_SamplingRate: TPanel;
    txt_SamplingRate: TLabel;
    cmb_SamplingRate: TComboBox;
    pnl_Bits: TPanel;
    txt_Bits: TLabel;
    cmb_Bits: TComboBox;
    pnl_Channel: TPanel;
    txt_Channel: TLabel;
    cmb_Channel: TComboBox;
    pnl_Control: TPanel;
    btn_StartCapture: TButton;
    btn_EndCapture: TButton;
    pnl_AudioType: TPanel;
    txt_AudioType: TLabel;
    cmb_AudioType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_StartCaptureClick(Sender: TObject);
    procedure btn_EndCaptureClick(Sender: TObject);
  private
    { Private êÈåæ }
    f_AudioStreamClientThread: TAudioStreamClientThread;

    procedure OnIdleApplication(Sender: TObject; var Done: Boolean);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdleApplication;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(f_AudioStreamClientThread) then
  begin
    FreeAndNil(f_AudioStreamClientThread);
  end;
end;

procedure TFormMain.OnIdleApplication(Sender: TObject; var Done: Boolean);
var
  l_AssignThread: Boolean;
begin
  l_AssignThread := Assigned(f_AudioStreamClientThread);

  btn_StartCapture.Enabled := not l_AssignThread;
  btn_EndCapture.Enabled := l_AssignThread;
end;

procedure TFormMain.btn_StartCaptureClick(Sender: TObject);
begin
  f_AudioStreamClientThread := TAudioStreamClientThread.Create(TAudioType(cmb_AudioType.ItemIndex),
    StrToInt(cmb_SamplingRate.Text), StrToInt(cmb_Bits.Text), cmb_Channel.ItemIndex + 1);
end;

procedure TFormMain.btn_EndCaptureClick(Sender: TObject);
begin
  FreeAndNil(f_AudioStreamClientThread);
end;

end.
