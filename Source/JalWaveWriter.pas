unit JalWaveWriter;

interface

uses
  System.Classes, System.SysUtils, Winapi.MMSystem,

  Jal.Win.AudioClient;

type
  TJalWaveWriter = class
  private
    f_Format: tWAVEFORMATEX;
    f_FileStream: TFileStream;
    f_BinaryWriter: TBinaryWriter;
    f_DataSizePosition: Int64;
    f_DataSize: UInt32;
  public
    constructor Create(const a_DirFileName: string; const a_Format: tWAVEFORMATEX);
    destructor Destroy; override;

    property Format: tWAVEFORMATEX read f_Format;

    procedure WriteBuffer(const a_pSource: PByte; const a_Count: Integer);
    procedure Close;
  end;

implementation

{ TWaveWriter }

constructor TJalWaveWriter.Create(const a_DirFileName: string; const a_Format: tWAVEFORMATEX);
begin
  f_Format := a_Format;

  // Create Streams
  f_FileStream := TFileStream.Create(a_DirFileName, fmCreate);
  f_BinaryWriter := TBinaryWriter.Create(f_FileStream, TEncoding.ASCII);

  // Write Headers
  f_BinaryWriter.Write('RIFF'.ToCharArray);
  f_BinaryWriter.Write(UInt32(0));
  f_BinaryWriter.Write('WAVE'.ToCharArray);
  f_BinaryWriter.Write('fmt '.ToCharArray);
  f_BinaryWriter.Write(UInt32(16 + a_Format.cbSize));
  f_BinaryWriter.Write(a_Format.wFormatTag);
  f_BinaryWriter.Write(a_Format.nChannels);
  f_BinaryWriter.Write(a_Format.nSamplesPerSec);
  f_BinaryWriter.Write(a_Format.nAvgBytesPerSec);
  f_BinaryWriter.Write(a_Format.nBlockAlign);
  f_BinaryWriter.Write(a_Format.wBitsPerSample);
  f_BinaryWriter.Write('data'.ToCharArray);
  f_DataSizePosition := f_FileStream.Position; // Store Position
  f_BinaryWriter.Write(UInt32(0));             // Reserve DataSize area

  f_DataSize := 0;
end;

destructor TJalWaveWriter.Destroy;
begin
  FreeAndNil(f_BinaryWriter);
  FreeAndNil(f_FileStream);

  inherited;
end;

procedure TJalWaveWriter.WriteBuffer(const a_pSource: PByte; const a_Count: Integer);
var
  l_Bytes: TBytes;
begin
  SetLength(l_Bytes, a_Count);
  Move(a_pSource^, l_Bytes[0], Length(l_Bytes));

  // Write to WAVE
  f_BinaryWriter.Write(l_Bytes, 0, a_Count);

  // Store data size
  Inc(f_DataSize, a_Count);
end;

procedure TJalWaveWriter.Close;
begin
  // Write Chunk
  f_BinaryWriter.Seek(4, TSeekOrigin.soBeginning);
  f_BinaryWriter.Write(UInt32(f_BinaryWriter.BaseStream.Size - 8));

  // Write Data Size
  f_BinaryWriter.Seek(f_DataSizePosition, TSeekOrigin.soBeginning);
  f_BinaryWriter.Write(f_DataSize);
end;

end.
