unit Form_Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,

  JalCaptureAudioThread, JalWaveWriter, Jal.Win.AudioClient, Jal.Win.MMDeviceAPI;

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
    f_CaptureAudioThread: TJalCaptureAudioThread;
    f_WaveWriter: TJalWaveWriter;

    procedure OnIdleApplication(Sender: TObject; var Done: Boolean);
    procedure OnDefaultDeviceChanged(const a_Flow: EDataFlow; const a_Role: ERole; const a_DeviceId: PWideChar);
    procedure OnCaptureBuffer(const a_Sender: TThread; const a_pData: PByte; const a_Count: Integer);
    procedure OnTerminate(Sender: TObject);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

uses
  JalWaveHelper;

{$R *.dfm}


procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdleApplication;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  btn_EndCaptureClick(Self);
end;

procedure TFormMain.OnIdleApplication(Sender: TObject; var Done: Boolean);
var
  l_isRunningThread: Boolean;
begin
  l_isRunningThread := Assigned(f_CaptureAudioThread);

  btn_StartCapture.Enabled := not l_isRunningThread;
  btn_EndCapture.Enabled := l_isRunningThread;
end;

procedure TFormMain.btn_StartCaptureClick(Sender: TObject);
var
  l_Format: WAVEFORMATEX;
begin
  // Get Format
  l_Format := TJalWaveHelper.GetPCMFormat(StrToInt(cmb_SamplingRate.Text), StrToInt(cmb_Bits.Text),
    cmb_Channel.ItemIndex + 1);

  // Create Wave Writer
  f_WaveWriter := TJalWaveWriter.Create(ExtractFileDir(Application.ExeName) + Format('\%dch%dhz%dbit.wav',
    [l_Format.nChannels, l_Format.nSamplesPerSec, l_Format.wBitsPerSample]), l_Format);

  // Create Capture Thread
  f_CaptureAudioThread := TJalCaptureAudioThread.Create(TAudioType(cmb_AudioType.ItemIndex), f_WaveWriter.Format,
    OnDefaultDeviceChanged);

  // Assign Handlers
  f_CaptureAudioThread.OnCaptureBuffer := OnCaptureBuffer;
  f_CaptureAudioThread.OnTerminate := OnTerminate;
end;

procedure TFormMain.btn_EndCaptureClick(Sender: TObject);
begin
  if Assigned(f_CaptureAudioThread) then
  begin
    f_CaptureAudioThread.OnTerminate := nil;
    f_CaptureAudioThread.Terminate;
    f_CaptureAudioThread.WaitFor;
    FreeAndNil(f_CaptureAudioThread);
  end;

  if Assigned(f_WaveWriter) then
  begin
    f_WaveWriter.Close;
    FreeAndNil(f_WaveWriter);
  end;
end;

procedure TFormMain.OnDefaultDeviceChanged(const a_Flow: EDataFlow; const a_Role: ERole; const a_DeviceId: PWideChar);
begin
  TThread.Queue(nil,
    procedure
    begin
      btn_EndCaptureClick(Self);
    end);
end;

procedure TFormMain.OnCaptureBuffer(const a_Sender: TThread; const a_pData: PByte; const a_Count: Integer);
begin
  if not a_Sender.CheckTerminated then
  begin
    // Write Buffer
    f_WaveWriter.WriteBuffer(a_pData, a_Count);
  end;
end;

procedure TFormMain.OnTerminate(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          btn_EndCaptureClick(Self);
        end);
    end).Start;
end;

end.
