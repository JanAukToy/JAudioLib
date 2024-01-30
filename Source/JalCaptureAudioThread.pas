unit JalCaptureAudioThread;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.StrUtils, Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
  Vcl.Forms,

  Jal.Win.MMDeviceAPI, Jal.Win.AudioClient, JalAudioDevice;

type
  TAudioType = (atMic, atSystem);
  TOnCaptureBuffer = procedure(const a_Sender: TThread; const a_pData: PByte; const a_Count: Integer) of object;

  TJalCaptureAudioThread = class(TThread)
  private
    f_AudioType: TAudioType;
    f_OnCaptureBuffer: TOnCaptureBuffer;

    f_AudioDevice: TJalAudioDevice;
    f_AudioClient: IAudioClient;
    f_pWaveFormat: PWAVEFORMATEX;
    f_AudioCaptureClient: IAudioCaptureClient;
    f_ActualDuration: REFERENCE_TIME;

    function StartCapture: Boolean;
  public
    constructor Create(const a_AudioType: TAudioType; const a_pFormat: PWAVEFORMATEX);
    destructor Destroy; override;

    property OnCaptureBuffer: TOnCaptureBuffer write f_OnCaptureBuffer;
  protected
    procedure Execute; override;
  end;

const
  // 1 REFTIMES = 100 nano sec
  REFTIMES_PER_SEC: REFERENCE_TIME = 10000000; // REFTIMES to Sec
  REFTIMES_PER_MSEC: REFERENCE_TIME = 10000; // REFTIMES to MSec

implementation

uses
  JalWaveHelper;

{ TAudioStreamClientThread }

constructor TJalCaptureAudioThread.Create(const a_AudioType: TAudioType; const a_pFormat: PWAVEFORMATEX);
begin
  f_AudioType := a_AudioType;
  f_pWaveFormat := a_pFormat;

  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TJalCaptureAudioThread.Destroy;
begin
  if Assigned(f_AudioDevice) then
    FreeAndNil(f_AudioDevice);

  inherited;
end;

function TJalCaptureAudioThread.StartCapture: Boolean;
var
  l_StreamFlags: DWORD;
  l_BufferFrameCount: DWORD;
begin
  Result := False;

  // Get Audio Client
  if Succeeded(f_AudioDevice.Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil, f_AudioClient)) then
  begin
    case f_AudioType of
      atMic:
        l_StreamFlags := AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY;
      atSystem:
        l_StreamFlags := AUDCLNT_STREAMFLAGS_LOOPBACK or AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
          AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY;
    else
      l_StreamFlags := 0;
    end;

    // Init AudioClient *AUTOCONVERTPCM makes the IsFormatSupported and GetMixFormat function unnecessary.
    if Succeeded(f_AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, l_StreamFlags, REFTIMES_PER_SEC, 0, f_pWaveFormat,
      nil)) then
    begin
      // Get Buffer Size
      if Succeeded(f_AudioClient.GetBufferSize(@l_BufferFrameCount)) then
      begin
        // Get Duration
        f_ActualDuration := Round(REFTIMES_PER_SEC * l_BufferFrameCount / f_pWaveFormat.nSamplesPerSec);

        // Get Audio Capture Client
        if Succeeded(f_AudioClient.GetService(IID_IAudioCaptureClient, f_AudioCaptureClient)) then
        begin
          // Start Capture
          Result := Succeeded(f_AudioClient.Start);
        end;
      end;
    end;
  end;
end;

procedure TJalCaptureAudioThread.Execute;
var
  l_DataFlow: EDataFlow;
  l_IncomingBufferSize: UInt32;
  l_PacketLength: UInt32;
  l_pBuffer: PByte;
  l_NumFramesAvailable: UInt32;
  l_Flags: DWORD;
  l_DevicePosition: UInt64;
  l_QPCPosition: UInt64;
begin
  case f_AudioType of
    atMic:
      l_DataFlow := eCapture;
    atSystem:
      l_DataFlow := eRender;
  else
    Exit;
  end;

  // Create Audio Device
  f_AudioDevice := TJalAudioDevice.Create(COINIT_MULTITHREADED, l_DataFlow);

  // Check ready device and start capture
  if (f_AudioDevice.Ready) and (StartCapture) then
  begin
    while (not Terminated) do
    begin
      // Wait...
      TThread.Sleep(Round(f_ActualDuration / REFTIMES_PER_MSEC / 2));

      // Get packet size
      if Succeeded(f_AudioCaptureClient.GetNextPacketSize(@l_PacketLength)) then
      begin
        // Process all packet
        while l_PacketLength <> 0 do
        begin
          l_pBuffer := nil;

          // Get buffer pointer
          if Succeeded(f_AudioCaptureClient.GetBuffer(l_pBuffer, @l_NumFramesAvailable, @l_Flags, @l_DevicePosition,
            @l_QPCPosition)) then
          begin
            // Check sirent
            if (l_Flags and Ord(AUDCLNT_BUFFERFLAGS_SILENT)) > 0 then
            begin
              l_pBuffer := nil;
            end;

            if (l_pBuffer <> nil) and (l_NumFramesAvailable > 0) then
            begin
              l_IncomingBufferSize := f_pWaveFormat.nBlockAlign * l_NumFramesAvailable;

              // Callback Capture Buffer
              if Assigned(f_OnCaptureBuffer) then
                f_OnCaptureBuffer(Self, l_pBuffer, l_IncomingBufferSize);
            end;

            if Succeeded(f_AudioCaptureClient.ReleaseBuffer(l_NumFramesAvailable)) then
            begin
              // Get next packet size
              f_AudioCaptureClient.GetNextPacketSize(@l_PacketLength);
            end;
          end;
        end;
      end;
    end;

    // Stop Capture
    f_AudioClient.Stop;
  end;
end;

end.
