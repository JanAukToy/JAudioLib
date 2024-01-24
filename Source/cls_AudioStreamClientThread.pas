unit cls_AudioStreamClientThread;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.StrUtils, Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
  Vcl.Forms,

  JAT.MMDeviceAPI, JAT.AudioClient, cls_AudioStreamDevice;

type
  TAudioType = (atMic, atSystem);

  TAudioStreamClientThread = class(TThread)
  private
    f_AudioType: TAudioType;
    f_StreamDevice: TAudioStreamDevice;
    f_AudioClient: IAudioClient;
    f_pWaveFormat: PWAVEFORMATEX;
    f_AudioCaptureClient: IAudioCaptureClient;
    f_ActualDuration: REFERENCE_TIME;

    function StartCapture: Boolean;
    function GetWaveFormatText: string;

    procedure Execute_CreateWAVE;
  public
    constructor Create(const a_AudioType: TAudioType; const a_Samples: Cardinal; const a_Bits, a_Channels: Word);
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

constructor TAudioStreamClientThread.Create(const a_AudioType: TAudioType; const a_Samples: Cardinal;
  const a_Bits, a_Channels: Word);
begin
  f_AudioType := a_AudioType;

  // Set Format
  GetMem(f_pWaveFormat, SizeOf(tWAVEFORMATEX));
  f_pWaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  f_pWaveFormat.nChannels := a_Channels;
  f_pWaveFormat.nSamplesPerSec := a_Samples;
  f_pWaveFormat.wBitsPerSample := a_Bits;
  f_pWaveFormat.nBlockAlign := Round(f_pWaveFormat.nChannels * f_pWaveFormat.wBitsPerSample / 8);
  f_pWaveFormat.nAvgBytesPerSec := f_pWaveFormat.nSamplesPerSec * f_pWaveFormat.nBlockAlign;
  f_pWaveFormat.cbSize := 0;

  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TAudioStreamClientThread.Destroy;
begin
  if Assigned(f_StreamDevice) then
  begin
    FreeAndNil(f_StreamDevice);
  end;

  FreeMem(f_pWaveFormat, SizeOf(f_pWaveFormat^));

  inherited;
end;

function TAudioStreamClientThread.StartCapture: Boolean;
var
  l_PointAudioClient: Pointer;
  l_StreamFlags: Cardinal;
  l_BufferFrameCount: Cardinal;
  l_PointAudioCaptureClient: Pointer;
begin
  Result := False;

  // Get Audio Client
  if Succeeded(f_StreamDevice.Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil, l_PointAudioClient)) then
  begin
    f_AudioClient := IAudioClient(l_PointAudioClient) as IAudioClient;

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
      TGuid.Empty)) then
    begin
      // Get Buffer Size
      if Succeeded(f_AudioClient.GetBufferSize(l_BufferFrameCount)) then
      begin
        // Get Duration
        f_ActualDuration := Round(REFTIMES_PER_SEC * l_BufferFrameCount / f_pWaveFormat.nSamplesPerSec);

        // Get Audio Capture Client
        if Succeeded(f_AudioClient.GetService(IID_IAudioCaptureClient, l_PointAudioCaptureClient)) then
        begin
          f_AudioCaptureClient := IAudioCaptureClient(l_PointAudioCaptureClient) as IAudioCaptureClient;

          // Start Capture
          Result := Succeeded(f_AudioClient.Start);
        end;
      end;
    end;

  end;
end;

function TAudioStreamClientThread.GetWaveFormatText: string;
begin
  Result := Format('%dch%dhz%dbit', [f_pWaveFormat.nChannels, f_pWaveFormat.nSamplesPerSec,
    f_pWaveFormat.wBitsPerSample]);
end;

procedure TAudioStreamClientThread.Execute;
begin
  Execute_CreateWAVE;
end;

procedure TAudioStreamClientThread.Execute_CreateWAVE;
var
  l_FileStream: TFileStream;
  l_BinaryWriter: TBinaryWriter;
  l_Bytes: TBytes;
  l_DataSizePosition: Int64;
  l_ChunkSize: UInt32;
  l_IncomingBufferSize: UInt32;

  l_DataFlow: EDataFlow;
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
    l_DataFlow := EDataFlow(0);
  end;

  // Create Audio Stream Device
  f_StreamDevice := TAudioStreamDevice.Create(COINIT_MULTITHREADED, l_DataFlow);

  // Check Ready Device and Start Capture
  if (f_StreamDevice.Ready) and (StartCapture) then
  begin
    l_FileStream := TFileStream.Create(ExtractFileDir(Application.ExeName) + Format('\%s.wav', [GetWaveFormatText]),
      fmCreate);
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
