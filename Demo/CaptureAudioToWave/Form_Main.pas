unit Form_Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,

  JAT.CaptureAudioThread, JAT.WaveWriter, JAT.Win.AudioClient;

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
    f_CaptureAudioThread: TCaptureAudioThread;
    f_WaveWriter: TWaveWriter;

    procedure OnIdleApplication(Sender: TObject; var Done: Boolean);
    procedure OnStartCapture(const a_pFormat: PWAVEFORMATEX);
    procedure OnEndCapture(const a_AllDataSize: UInt32);
    procedure OnCaptureBuffer(const a_pData: PByte; const a_Count: Integer);
    procedure OnTerminate(Sender: TObject);
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
  if Assigned(f_CaptureAudioThread) then
    f_CaptureAudioThread.Terminate;
end;

procedure TFormMain.OnIdleApplication(Sender: TObject; var Done: Boolean);
var
  l_isRunningThread: Boolean;
begin
  l_isRunningThread := Assigned(f_CaptureAudioThread) and (not f_CaptureAudioThread.Finished);

  btn_StartCapture.Enabled := not l_isRunningThread;
  btn_EndCapture.Enabled := l_isRunningThread;
end;

procedure TFormMain.btn_StartCaptureClick(Sender: TObject);
begin
  // Create Capture Thread
  f_CaptureAudioThread := TCaptureAudioThread.Create(TAudioType(cmb_AudioType.ItemIndex),
    StrToInt(cmb_SamplingRate.Text), StrToInt(cmb_Bits.Text), cmb_Channel.ItemIndex + 1);

  // Assign Handlers
  f_CaptureAudioThread.OnStartCapture := OnStartCapture;
  f_CaptureAudioThread.OnEndCapture := OnEndCapture;
  f_CaptureAudioThread.OnCaptureBuffer := OnCaptureBuffer;
  f_CaptureAudioThread.OnTerminate := OnTerminate;
end;

procedure TFormMain.btn_EndCaptureClick(Sender: TObject);
begin
  if Assigned(f_CaptureAudioThread) then
  begin
    f_CaptureAudioThread.Terminate;
    f_CaptureAudioThread := nil;
  end;
end;

procedure TFormMain.OnStartCapture(const a_pFormat: PWAVEFORMATEX);
begin
  // Create Wave Writer
  f_WaveWriter := TWaveWriter.Create(ExtractFileDir(Application.ExeName) + Format('\%dch%dhz%dbit.wav',
    [a_pFormat.nChannels, a_pFormat.nSamplesPerSec, a_pFormat.wBitsPerSample]), a_pFormat);
end;

procedure TFormMain.OnEndCapture(const a_AllDataSize: UInt32);
begin
  // Close Wave Writer
  f_WaveWriter.Close(a_AllDataSize);
end;

procedure TFormMain.OnCaptureBuffer(const a_pData: PByte; const a_Count: Integer);
begin
  // Write Buffer
  f_WaveWriter.WriteBuffer(a_pData, a_Count);
end;

procedure TFormMain.OnTerminate(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          FreeAndNil(f_CaptureAudioThread);
          FreeAndNil(f_WaveWriter);
        end);
    end).Start;
end;

end.
