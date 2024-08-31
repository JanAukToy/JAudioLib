unit JalWaveReader;

interface

uses
  System.Classes, System.SysUtils, Winapi.MMSystem,

  Jal.Win.AudioClient;

type
  TJalWaveReader = class
  private
    f_Available: Boolean;
    f_FileStream: TFileStream;
    f_BinaryReader: TBinaryReader;
    f_Format: WAVEFORMATEX;
  public
    constructor Create(const a_DirFileName: string);
    destructor Destroy; override;

    property Available: Boolean read f_Available;
    property Format: WAVEFORMATEX read f_Format;

    function ReadBuffer(const a_pDest: PByte; const a_Count: Cardinal): Cardinal;
  end;

const
  WAVE_FORMAT_EXTENSIBLE = $FFFE;

implementation

{ TWaveReader }

constructor TJalWaveReader.Create(const a_DirFileName: string);
var
  l_RIFF: TArray<Char>;
  l_RIFFStr: string;
  l_ChunkSize: Cardinal;
  l_Format: TArray<Char>;
  l_FormatStr: string;
  l_FmtIdent: TArray<Char>;
  l_FmtIdentStr: string;
  l_FmtSize: Cardinal;
  l_DataIdent: TArray<Char>;
  l_DataIdentStr: string;
begin
  f_Available := False;

  // Create Streams
  f_FileStream := TFileStream.Create(a_DirFileName, fmOpenRead);
  f_BinaryReader := TBinaryReader.Create(f_FileStream, TEncoding.ASCII);

  try
    // Read Headers...
    l_RIFF := f_BinaryReader.ReadChars(4);
    l_ChunkSize := f_BinaryReader.ReadCardinal;
    l_Format := f_BinaryReader.ReadChars(4);

    SetString(l_RIFFStr, PChar(l_RIFF), Length(l_RIFF));
    SetString(l_FormatStr, PChar(l_Format), Length(l_Format));

    // Check Headers
    if (l_RIFFStr = 'RIFF') and (l_FormatStr = 'WAVE') then
    begin
      // Read fmt chunks...
      l_FmtIdent := f_BinaryReader.ReadChars(4);
      l_FmtSize := f_BinaryReader.ReadCardinal;

      SetString(l_FmtIdentStr, PChar(l_FmtIdent), Length(l_FmtIdent));

      if (l_FmtIdentStr = 'fmt ') and (l_FmtSize >= 16) then
      begin
        f_Format.wFormatTag := f_BinaryReader.ReadWord;
        f_Format.nChannels := f_BinaryReader.ReadWord;
        f_Format.nSamplesPerSec := f_BinaryReader.ReadCardinal;
        f_Format.nAvgBytesPerSec := f_BinaryReader.ReadCardinal;
        f_Format.nBlockAlign := f_BinaryReader.ReadWord;
        f_Format.wBitsPerSample := f_BinaryReader.ReadWord;
      end;

      // PCM
      if f_Format.wFormatTag = WAVE_FORMAT_PCM then
      begin
        // Without extension block
        if l_FmtSize = 16 then
        begin
          f_Format.cbSize := 0;

          // Read Data chunk
          l_DataIdent := f_BinaryReader.ReadChars(4);

          SetString(l_DataIdentStr, PChar(l_DataIdent), Length(l_DataIdent));

          if l_DataIdentStr = 'data' then
          begin
            f_Available := True;
          end;
        end;
      end
      // Extensible
      else
        if f_Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE then
        begin
          // Read extension block...
          f_Format.cbSize := f_BinaryReader.ReadWord;

          { TODO : Support for WAVEFORMATEXTENSIBLE format }
        end;
    end;

  except
    on E: Exception do
    begin

    end;
  end;
end;

destructor TJalWaveReader.Destroy;
begin
  FreeAndNil(f_BinaryReader);
  FreeAndNil(f_FileStream);

  inherited;
end;

function TJalWaveReader.ReadBuffer(const a_pDest: PByte; const a_Count: Cardinal): Cardinal;
var
  l_Data: TBytes;
begin
  // Read Data
  SetLength(l_Data, a_Count);
  Result := f_BinaryReader.Read(l_Data, 0, a_Count);

  Move(l_Data[0], a_pDest^, Result);
end;

end.
