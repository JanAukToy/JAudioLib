unit cls_AudioStreamClientThread;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.StrUtils, Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
  Vcl.Forms,

  JAT.MMDeviceAPI, JAT.AudioClient;

type
  TAudioStreamClientThread = class(TThread)
  private
    f_Started: Boolean;
    f_Device: IMMDevice;

    f_AudioClient: IAudioClient;
    f_WaveFormat: WAVEFORMATEX;
    f_AudioCaptureClient: IAudioCaptureClient;
    f_ActualDuration: REFERENCE_TIME;

    function StartCapture: Boolean;
  public
    constructor Create(const a_Device: IMMDevice);
    destructor Destroy; override;
  protected
    procedure Execute; override;
  end;

const
  // 1 REFTIMES = 100 nano sec
  REFTIMES_PER_SEC: REFERENCE_TIME = 10000000; // REFTIMES to Sec
  REFTIMES_PER_MSEC: REFERENCE_TIME = 10000; // REFTIMES to MSec

implementation

{ TAudioStreamClientThread }

constructor TAudioStreamClientThread.Create(const a_Device: IMMDevice);
begin
  f_Device := a_Device;

  f_Started := StartCapture;

  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TAudioStreamClientThread.Destroy;
begin
  if f_Started then
  begin
    f_AudioClient.Stop;
  end;

  inherited;
end;

function TAudioStreamClientThread.StartCapture: Boolean;
const
  WAVE_FORMAT_IEEE_FLOAT = $0003;
var
  l_PointAudioClient: Pointer;
  l_BufferFrameCount: Cardinal;
  l_PointAudioCaptureClient: Pointer;
  l_pCloseWaveFormat: WAVEFORMATEX;
begin
  Result := False;

  // Get Audio Client
  if Succeeded(f_Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil, l_PointAudioClient)) then
  begin
    f_AudioClient := IAudioClient(l_PointAudioClient) as IAudioClient;

    // Create Wave Format
    f_WaveFormat.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
    f_WaveFormat.nChannels := 2;
    f_WaveFormat.nSamplesPerSec := 48000;
    f_WaveFormat.wBitsPerSample := 32;
    f_WaveFormat.cbSize := 0;
    f_WaveFormat.nBlockAlign := Round(f_WaveFormat.nChannels * f_WaveFormat.wBitsPerSample / 8);
    f_WaveFormat.nAvgBytesPerSec := f_WaveFormat.nSamplesPerSec * f_WaveFormat.nBlockAlign;

    // Check Support Format
    if Succeeded(f_AudioClient.IsFormatSupported(AUDCLNT_SHAREMODE_SHARED, f_WaveFormat, l_pCloseWaveFormat)) then
    begin
      // Init AudioClient
      if Succeeded(f_AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, 0, REFTIMES_PER_SEC, 0, f_WaveFormat, TGUID.Empty))
      then
      begin
        // Get Buffer Size
        if Succeeded(f_AudioClient.GetBufferSize(l_BufferFrameCount)) then
        begin
          // Get Audio Capture Client
          if Succeeded(f_AudioClient.GetService(IID_IAudioCaptureClient, l_PointAudioCaptureClient)) then
          begin
            f_AudioCaptureClient := IAudioCaptureClient(l_PointAudioCaptureClient) as IAudioCaptureClient;

            // Get Duration
            f_ActualDuration := Round(REFTIMES_PER_SEC * l_BufferFrameCount / f_WaveFormat.nSamplesPerSec);

            // Start Capture
            Result := Succeeded(f_AudioClient.Start);
          end;
        end;
      end;
    end;
  end;
end;

procedure TAudioStreamClientThread.Execute;
var
  l_FileStream: TFileStream;
  l_BinaryWriter: TBinaryWriter;
  l_Bytes: TBytes;
  l_DataSizePosition: Int64;
  l_ChunkSize: UInt32;
  l_IncomingBufferSize: UInt32;

  l_PacketLength: UInt32;
  l_pBuffer: PByte;
  l_NumFramesAvailable: UInt32;
  l_Flags: DWORD;
  l_DevicePosition: UInt64;
  l_QPCPosition: UInt64;
begin
  // Init COM
  if (not f_Started) or (not Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED))) then
  begin
    Exit;
  end;

  try
    l_FileStream := TFileStream.Create(ExtractFileDir(Application.ExeName) + '\capturesample.wav', fmCreate);

    l_BinaryWriter := TBinaryWriter.Create(l_FileStream);

    try
      l_BinaryWriter.Write('RIFF'.ToCharArray);
      l_BinaryWriter.Write(UInt32(0));
      l_BinaryWriter.Write('WAVE'.ToCharArray);
      l_BinaryWriter.Write('fmt '.ToCharArray);
      l_BinaryWriter.Write(UInt32(18 + f_WaveFormat.cbSize));
      l_BinaryWriter.Write(f_WaveFormat.wFormatTag);
      l_BinaryWriter.Write(f_WaveFormat.nChannels);
      l_BinaryWriter.Write(f_WaveFormat.nSamplesPerSec);
      l_BinaryWriter.Write(f_WaveFormat.nAvgBytesPerSec);
      l_BinaryWriter.Write(f_WaveFormat.nBlockAlign);
      l_BinaryWriter.Write(f_WaveFormat.wBitsPerSample);
      l_BinaryWriter.Write(f_WaveFormat.cbSize);
      l_BinaryWriter.Write('data'.ToCharArray);
      l_DataSizePosition := l_FileStream.Position;
      l_BinaryWriter.Write(UInt32(0));

      l_ChunkSize := 0;

      while (not Terminated) do
      begin
        // Wait...
        TThread.Sleep(Round(f_ActualDuration / REFTIMES_PER_MSEC / 2));

        // Get Packet Size
        if Succeeded(f_AudioCaptureClient.GetNextPacketSize(l_PacketLength)) then
        begin
          // Process All Packet
          while l_PacketLength <> 0 do
          begin
            l_pBuffer := nil;

            // Get Buffer Pointer
            if Succeeded(f_AudioCaptureClient.GetBuffer(l_pBuffer, l_NumFramesAvailable, l_Flags, l_DevicePosition,
              l_QPCPosition)) then
            begin
              // Check Non Audio
              if (l_Flags and Ord(AUDCLNT_BUFFERFLAGS_SILENT)) > 0 then
              begin
                l_pBuffer := nil;
              end;

              if (l_pBuffer <> nil) and (l_NumFramesAvailable > 0) then
              begin
                l_IncomingBufferSize := f_WaveFormat.nBlockAlign * l_NumFramesAvailable;

                // Copy Buffer
                SetLength(l_Bytes, l_IncomingBufferSize);
                Move(l_pBuffer^, l_Bytes[0], Length(l_Bytes));

                // Write to WAVE
                l_BinaryWriter.Write(l_Bytes, 0, Length(l_Bytes));

                Inc(l_ChunkSize, Length(l_Bytes));
              end;

              if Succeeded(f_AudioCaptureClient.ReleaseBuffer(l_NumFramesAvailable)) then
              begin
                // Get Next Packet Size
                f_AudioCaptureClient.GetNextPacketSize(l_PacketLength);
              end;
            end;
          end;
        end;
      end;

      // Stop Capture
      if f_AudioClient.Stop = S_OK then
      begin
        f_Started := False;
      end;

      l_BinaryWriter.Seek(4, TSeekOrigin.soBeginning);
      l_BinaryWriter.Write(UInt32(l_BinaryWriter.BaseStream.Size - 8));

      l_BinaryWriter.Seek(l_DataSizePosition, TSeekOrigin.soBeginning);
      l_BinaryWriter.Write(l_ChunkSize);

    finally
      l_BinaryWriter.Free;
      l_FileStream.Free;
    end;

  finally
    CoUninitialize();
  end;
end;

end.
