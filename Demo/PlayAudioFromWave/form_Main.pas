unit form_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  JalRenderAudioThread, JalWaveReader, Jal.Win.AudioClient;

type
  TFormMain = class(TForm)
    btn_StartPlay: TButton;
    pnl_Settings: TPanel;
    pnl_DirWaveFile: TPanel;
    pnl_Control: TPanel;
    btn_EndPlay: TButton;
    txt_DirWaveFile: TLabel;
    edt_DirWaveFile: TEdit;
    odl_Wave: TOpenDialog;
    btn_OpenWaveFile: TButton;
    procedure btn_StartPlayClick(Sender: TObject);
    procedure btn_OpenWaveFileClick(Sender: TObject);
    procedure btn_EndPlayClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private êÈåæ }
    f_RenderAudioThread: TJalRenderAudioThread;
    f_WaveReader: TJalWaveReader;

    procedure OnIdleApplication(Sender: TObject; var Done: Boolean);
    procedure OnRenderBuffer(const a_Sender: TThread; const a_pData: PByte; const a_AvailableCount: Cardinal;
      var a_Flags: DWORD);
    procedure OnTerminate(Sender: TObject);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Math;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdleApplication;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(f_RenderAudioThread) then
    f_RenderAudioThread.Terminate;
end;

procedure TFormMain.OnIdleApplication(Sender: TObject; var Done: Boolean);
var
  l_isRunningThread: Boolean;
begin
  l_isRunningThread := Assigned(f_RenderAudioThread);

  btn_StartPlay.Enabled := not l_isRunningThread;
  btn_EndPlay.Enabled := l_isRunningThread;
end;

procedure TFormMain.btn_OpenWaveFileClick(Sender: TObject);
begin
  if odl_Wave.Execute then
  begin
    edt_DirWaveFile.Text := odl_Wave.FileName;
  end;
end;

procedure TFormMain.btn_StartPlayClick(Sender: TObject);
begin
  if FileExists(edt_DirWaveFile.Text) then
  begin
    // Create Wave Reader
    f_WaveReader := TJalWaveReader.Create(edt_DirWaveFile.Text);

    if f_WaveReader.Available then
    begin
      // Create Render Thread
      f_RenderAudioThread := TJalRenderAudioThread.Create(f_WaveReader.Format);
      f_RenderAudioThread.OnRenderBuffer := OnRenderBuffer;
      f_RenderAudioThread.OnTerminate := OnTerminate;
    end;
  end;
end;

procedure TFormMain.btn_EndPlayClick(Sender: TObject);
begin
  if Assigned(f_RenderAudioThread) then
    f_RenderAudioThread.Terminate;
end;

procedure TFormMain.OnRenderBuffer(const a_Sender: TThread; const a_pData: PByte; const a_AvailableCount: Cardinal;
  var a_Flags: DWORD);
begin
  if not a_Sender.CheckTerminated then
  begin
    // Read buffer and set flags
    a_Flags := IfThen(f_WaveReader.ReadBuffer(a_pData, a_AvailableCount) = 0, Ord(AUDCLNT_BUFFERFLAGS_SILENT), 0);

    // End of data
    if a_Flags = DWORD(Ord(AUDCLNT_BUFFERFLAGS_SILENT)) then
    begin
      a_Sender.Terminate;
    end;
  end;
end;

procedure TFormMain.OnTerminate(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      FreeAndNil(f_WaveReader);
      f_RenderAudioThread := nil;
    end);
end;

end.
