unit JalWaveHelper;

interface

uses
  Winapi.MMSystem,
  Jal.Win.AudioClient;

type
  TJalWaveHelper = class
  public
    class function GetPCMFormat(const a_Samples: Cardinal; const a_Bits, a_Channels: Word): tWAVEFORMATEX;
  end;

  TMyWaveformatExHelper = record helper for tWAVEFORMATEX
    function ToExtensible: WAVEFORMATEXTENSIBLE;
  end;

implementation

{ TJalWaveHelper }

class function TJalWaveHelper.GetPCMFormat(const a_Samples: Cardinal; const a_Bits, a_Channels: Word): tWAVEFORMATEX;
begin
  Result.wFormatTag := WAVE_FORMAT_PCM;
  Result.nChannels := a_Channels;
  Result.nSamplesPerSec := a_Samples;
  Result.wBitsPerSample := a_Bits;
  Result.nBlockAlign := Round(Result.nChannels * Result.wBitsPerSample / 8);
  Result.nAvgBytesPerSec := Result.nSamplesPerSec * Result.nBlockAlign;
  Result.cbSize := 0;
end;

{ TMyWaveformatExHelper }

function TMyWaveformatExHelper.ToExtensible: WAVEFORMATEXTENSIBLE;
begin
  Result.Format := Self;

  Result.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
  Result.Format.cbSize := 22;
  Result.wValidBitsPerSample := Result.Format.wBitsPerSample;
  if Self.nChannels = 1 then
  begin
    Result.dwChannelMask := SPEAKER_FRONT_LEFT;
  end
  else if Self.nChannels = 2 then
  begin
    Result.dwChannelMask := SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT;
  end;
  Result.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
end;

end.
