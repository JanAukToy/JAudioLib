unit JAT.WaveWriter;

interface

uses
  System.Classes, System.SysUtils,

  JAT.Win.AudioClient;

type
  TWaveWriter = class
  private
    f_FileStream: TFileStream;
    f_BinaryWriter: TBinaryWriter;
    f_DataSizePosition: Int64;
  public
    constructor Create(const a_DirFileName: string; const a_pFormat: PWAVEFORMATEX);
    destructor Destroy; override;

    procedure WriteBuffer(const a_pBytes: PByte; const a_Count: Integer);
    procedure Close(const a_DataSize: UInt32);
  end;

implementation

{ TWaveWriter }

constructor TWaveWriter.Create(const a_DirFileName: string; const a_pFormat: PWAVEFORMATEX);
begin
  // Create Streams
  f_FileStream := TFileStream.Create(a_DirFileName, fmCreate);
  f_BinaryWriter := TBinaryWriter.Create(f_FileStream);

  // Write Headers
  f_BinaryWriter.Write('RIFF'.ToCharArray);
  f_BinaryWriter.Write(UInt32(0));
  f_BinaryWriter.Write('WAVE'.ToCharArray);
  f_BinaryWriter.Write('fmt '.ToCharArray);
  f_BinaryWriter.Write(UInt32(18 + a_pFormat.cbSize));
  f_BinaryWriter.Write(a_pFormat.wFormatTag);
  f_BinaryWriter.Write(a_pFormat.nChannels);
  f_BinaryWriter.Write(a_pFormat.nSamplesPerSec);
  f_BinaryWriter.Write(a_pFormat.nAvgBytesPerSec);
  f_BinaryWriter.Write(a_pFormat.nBlockAlign);
  f_BinaryWriter.Write(a_pFormat.wBitsPerSample);
  f_BinaryWriter.Write(a_pFormat.cbSize);
  f_BinaryWriter.Write('data'.ToCharArray);
  f_DataSizePosition := f_FileStream.Position; // Store Position
  f_BinaryWriter.Write(UInt32(0));
end;

destructor TWaveWriter.Destroy;
begin
  FreeAndNil(f_BinaryWriter);
  FreeAndNil(f_FileStream);

  inherited;
end;

procedure TWaveWriter.WriteBuffer(const a_pBytes: PByte; const a_Count: Integer);
var
  l_Bytes: TBytes;
begin
  SetLength(l_Bytes, a_Count);
  Move(a_pBytes^, l_Bytes[0], Length(l_Bytes));

  // Write to WAVE
  f_BinaryWriter.Write(l_Bytes, 0, a_Count);
end;

procedure TWaveWriter.Close(const a_DataSize: UInt32);
begin
  // Write Chunk
  f_BinaryWriter.Seek(4, TSeekOrigin.soBeginning);
  f_BinaryWriter.Write(UInt32(f_BinaryWriter.BaseStream.Size - 8));

  // Write Data Size
  f_BinaryWriter.Seek(f_DataSizePosition, TSeekOrigin.soBeginning);
  f_BinaryWriter.Write(a_DataSize);
end;

end.
