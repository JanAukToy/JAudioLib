unit JalWaveHelper;

interface

uses
  Winapi.MMSystem,
  JAT.Win.AudioClient;

type
  TJalWaveHelper = class
  public
    class function GetPCMFormat(const a_Samples: Cardinal; const a_Bits, a_Channels: Word): WAVEFORMATEX;
  end;

implementation

{ TJalWaveHelper }

class function TJalWaveHelper.GetPCMFormat(const a_Samples: Cardinal; const a_Bits, a_Channels: Word): WAVEFORMATEX;
begin
  Result.wFormatTag := WAVE_FORMAT_PCM;
  Result.nChannels := a_Channels;
  Result.nSamplesPerSec := a_Samples;
  Result.wBitsPerSample := a_Bits;
  Result.nBlockAlign := Round(Result.nChannels * Result.wBitsPerSample / 8);
  Result.nAvgBytesPerSec := Result.nSamplesPerSec * Result.nBlockAlign;
  Result.cbSize := 0;
end;

end.
