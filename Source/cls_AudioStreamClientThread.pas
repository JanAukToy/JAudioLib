unit cls_AudioStreamClientThread;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.StrUtils, Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
  Vcl.Forms,

  JAT.MMDeviceAPI, JAT.AudioClient, cls_AudioStreamDeviceManager;

type
  TAudioStreamClientThread = class(TThread)
  private
    f_AudioStreamDeviceManager: TAudioStreamDeviceManager;
    f_AudioClient: IAudioClient;
    f_pWaveFormat: PWAVEFORMATEX;
    f_AudioCaptureClient: IAudioCaptureClient;
    f_ActualDuration: REFERENCE_TIME;

    function StartCapture: Boolean;
  public
    constructor Create;
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

constructor TAudioStreamClientThread.Create;
begin
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TAudioStreamClientThread.Destroy;
begin
  if Assigned(f_AudioStreamDeviceManager) then
  begin
    FreeAndNil(f_AudioStreamDeviceManager);
  end;

  inherited;
end;

function TAudioStreamClientThread.StartCapture: Boolean;
var
  l_PointAudioClient: Pointer;
  l_BufferFrameCount: Cardinal;
  l_PointAudioCaptureClient: Pointer;
  l_pCloseWaveFormat: PWAVEFORMATEX;
begin
  Result := False;

  // Get Audio Client
  if Succeeded(f_AudioStreamDeviceManager.CaptureDevice.Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil,
    l_PointAudioClient)) then
  begin
    f_AudioClient := IAudioClient(l_PointAudioClient) as IAudioClient;

    // Get Base Format
    if Succeeded(f_AudioClient.GetMixFormat(f_pWaveFormat)) then
    begin
      // Fix Format
      f_pWaveFormat.wFormatTag := WAVE_FORMAT_PCM;
      f_pWaveFormat.cbSize := 0;

      // Check Support Format
      if Succeeded(f_AudioClient.IsFormatSupported(AUDCLNT_SHAREMODE_SHARED, f_pWaveFormat, l_pCloseWaveFormat)) then
      begin
        // Init AudioClient
        if Succeeded(f_AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, 0, REFTIMES_PER_SEC, 0, f_pWaveFormat,
          TGuid.Empty)) then
        begin
          // Get Buffer Size
          if Succeeded(f_AudioClient.GetBufferSize(l_BufferFrameCount)) then
          begin
            // Get Audio Capture Client
            if Succeeded(f_AudioClient.GetService(IID_IAudioCaptureClient, l_PointAudioCaptureClient)) then
            begin
              f_AudioCaptureClient := IAudioCaptureClient(l_PointAudioCaptureClient) as IAudioCaptureClient;

              // Get Duration
              f_ActualDuration := Round(REFTIMES_PER_SEC * l_BufferFrameCount / f_pWaveFormat.nSamplesPerSec);

              // Start Capture
              Result := Succeeded(f_AudioClient.Start);
            end;
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
  // Create Audio Stream Device Manager
  f_AudioStreamDeviceManager := TAudioStreamDeviceManager.Create(COINIT_MULTITHREADED);

  // Check Ready Device and Start Capture
  if (f_AudioStreamDeviceManager.CaptureDevice.Ready) and (StartCapture) then
  begin
    l_FileStream := TFileStream.Create(ExtractFileDir(Application.ExeName) + '\capturesample.wav', fmCreate);
    l_BinaryWriter := TBinaryWriter.Create(l_FileStream);

    try
      l_BinaryWriter.Write('RIFF'.ToCharArray);
      l_BinaryWriter.Write(UInt32(0));
      l_BinaryWriter.Write('WAVE'.ToCharArray);
      l_BinaryWriter.Write('fmt '.ToCharArray);
      l_BinaryWriter.Write(UInt32(18 + f_pWaveFormat.cbSize));
      l_BinaryWriter.Write(f_pWaveFormat.wFormatTag);
      l_BinaryWriter.Write(f_pWaveFormat.nChannels);
      l_BinaryWriter.Write(f_pWaveFormat.nSamplesPerSec);
      l_BinaryWriter.Write(f_pWaveFormat.nAvgBytesPerSec);
      l_BinaryWriter.Write(f_pWaveFormat.nBlockAlign);
      l_BinaryWriter.Write(f_pWaveFormat.wBitsPerSample);
      l_BinaryWriter.Write(f_pWaveFormat.cbSize);
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
                l_IncomingBufferSize := f_pWaveFormat.nBlockAlign * l_NumFramesAvailable;

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
      f_AudioClient.Stop;

      // Write Chunk
      l_BinaryWriter.Seek(4, TSeekOrigin.soBeginning);
      l_BinaryWriter.Write(UInt32(l_BinaryWriter.BaseStream.Size - 8));

      // Write Chunk Size
      l_BinaryWriter.Seek(l_DataSizePosition, TSeekOrigin.soBeginning);
      l_BinaryWriter.Write(l_ChunkSize);

    finally
      l_BinaryWriter.Free;
      l_FileStream.Free;
    end;
  end;
end;

end.
