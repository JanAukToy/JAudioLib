unit JalRenderAudioThread;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.StrUtils, Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
  Vcl.Forms,

  Jal.Win.MMDeviceAPI, Jal.Win.AudioClient, JalAudioDevice;

type
  TOnRenderBuffer = procedure(const a_Sender: TThread; const a_pData: PByte; const a_AvailableCount: Cardinal;
    var a_Flags: DWORD) of object;

  TJalRenderAudioThread = class(TThread)
  private
    f_WaveFormat: tWAVEFORMATEX;
    f_LowLatencyMode: Boolean;
    f_OnRenderBuffer: TOnRenderBuffer;

    f_AudioDevice: TJalAudioDevice;
    f_AudioClient: IAudioClient;
    f_AudioRenderClient: IAudioRenderClient;
    f_BufferFrameCount: Cardinal;
    f_ThreadIntervalMs: Cardinal;

    function StartRender: Boolean;
  public
    constructor Create(const a_Format: tWAVEFORMATEX; const a_LowLatencyMode: Boolean = False);
    destructor Destroy; override;

    property OnRenderBuffer: TOnRenderBuffer write f_OnRenderBuffer;
  protected
    procedure Execute; override;
  end;

const
  // 1 REFTIMES = 100 nano sec
  REFTIMES_PER_SEC: REFERENCE_TIME  = 10000000; // REFTIMES to Sec
  REFTIMES_PER_MSEC: REFERENCE_TIME = 10000;    // REFTIMES to MSec

  THREAD_INTERVALMS_LOWLATENCY: Integer = 10;

implementation

uses
  System.Math;

{ TRenderAudioThread }

constructor TJalRenderAudioThread.Create(const a_Format: tWAVEFORMATEX; const a_LowLatencyMode: Boolean = False);
begin
  f_WaveFormat := a_Format;
  f_LowLatencyMode := a_LowLatencyMode;

  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TJalRenderAudioThread.Destroy;
begin
  if Assigned(f_AudioDevice) then
    FreeAndNil(f_AudioDevice);

  inherited;
end;

function TJalRenderAudioThread.StartRender: Boolean;
var
  l_pBuffer: PByte;
  l_WaveFormatExtensible: WAVEFORMATEXTENSIBLE;
begin
  Result := False;

  // Get Audio Client
  if Succeeded(f_AudioDevice.Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil, f_AudioClient)) then
  begin
    // Change to extensible.
    l_WaveFormatExtensible.Format := f_WaveFormat;

    // Init AudioClient *AUTOCONVERTPCM makes the IsFormatSupported and GetMixFormat function unnecessary.
    if Succeeded(f_AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
      AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY, REFTIMES_PER_SEC, 0, @l_WaveFormatExtensible, nil)) then
    begin
      // Get buffer size
      if Succeeded(f_AudioClient.GetBufferSize(@f_BufferFrameCount)) then
      begin
        if f_LowLatencyMode then
        begin
          f_ThreadIntervalMs := THREAD_INTERVALMS_LOWLATENCY;
        end
        else
        begin
          // Get thread sleep time
          f_ThreadIntervalMs :=
            Ceil(REFTIMES_PER_SEC * f_BufferFrameCount / f_WaveFormat.nSamplesPerSec / REFTIMES_PER_MSEC / 2);
        end;

        // Get Audio Render Client
        if Succeeded(f_AudioClient.GetService(IID_IAudioRenderClient, f_AudioRenderClient)) then
        begin
          // Destoroy initial buffer
          if Succeeded(f_AudioRenderClient.GetBuffer(f_BufferFrameCount, l_pBuffer)) then
          begin
            if Succeeded(f_AudioRenderClient.ReleaseBuffer(f_BufferFrameCount, 0)) then
            begin
              // Start Render
              Result := Succeeded(f_AudioClient.Start);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TJalRenderAudioThread.Execute;
var
  l_NumFramesPadding: UInt32;
  l_NumFramesAvailable: UInt32;
  l_pBuffer: PByte;
  l_Flags: DWORD;
begin
  // Create Audio Device
  f_AudioDevice := TJalAudioDevice.Create(COINIT_MULTITHREADED, eRender);

  // Check ready device and start render
  if (f_AudioDevice.Ready) and (StartRender) then
  begin
    while (not Terminated) do
    begin
      // Wait...
      TThread.Sleep(f_ThreadIntervalMs);

      // See how much buffer space is available
      if Succeeded(f_AudioClient.GetCurrentPadding(@l_NumFramesPadding)) then
      begin
        l_NumFramesAvailable := f_BufferFrameCount - l_NumFramesPadding;

        // Get buffer space
        if Succeeded(f_AudioRenderClient.GetBuffer(l_NumFramesAvailable, l_pBuffer)) then
        begin
          // Callback Render Buffer
          // * Flags is AUDCLNT_BUFFERFLAGS_...
          if Assigned(f_OnRenderBuffer) then
            f_OnRenderBuffer(Self, l_pBuffer, l_NumFramesAvailable, l_Flags);

          f_AudioRenderClient.ReleaseBuffer(l_NumFramesAvailable, l_Flags);
        end;
      end;
    end;

    // Stop Capture
    f_AudioClient.Stop;
  end;
end;

end.
