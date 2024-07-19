unit Jal.Win.AudioClient;

interface

uses
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys;

const
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}';
  IID_IAudioRenderClient: TGUID = '{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}';
  IID_IAudioCaptureClient: TGUID = '{C8ADBD64-E71E-48a0-A4DE-185C395CD317}';

type
{$MINENUMSIZE 4}
  _AUDCLNT_SHAREMODE = (AUDCLNT_SHAREMODE_SHARED = $00000000, AUDCLNT_SHAREMODE_EXCLUSIVE = $00000001);
  AUDCLNT_SHAREMODE = _AUDCLNT_SHAREMODE;

  _AUDCLNT_BUFFERFLAGS = (AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY = $00000001, AUDCLNT_BUFFERFLAGS_SILENT = $00000002,
    AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR = $00000004);
{$MINENUMSIZE 1}
  REFERENCE_TIME = Int64;
  PREFERENCE_TIME = ^REFERENCE_TIME;

  PIAudioClient = ^IAudioClient;

  PWAVEFORMATEX = ^WAVEFORMATEX;

  tWAVEFORMATEX = record
    wFormatTag: WORD;
    nChannels: WORD;
    nSamplesPerSec: DWORD;
    nAvgBytesPerSec: DWORD;
    nBlockAlign: WORD;
    wBitsPerSample: WORD;
    cbSize: WORD;
  end;

  WAVEFORMATEX = tWAVEFORMATEX;

  IAudioClient = interface(IUnknown)
    ['{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}']
    function Initialize(ShareMode: AUDCLNT_SHAREMODE; StreamFlags: DWORD; hnsBufferDuration: REFERENCE_TIME;
      hnsPeriodicity: REFERENCE_TIME; const pFormat: PWAVEFORMATEX; const AudioSessionGuid: PGUID): HRESULT; stdcall;
    function GetBufferSize(pNumBufferFrames: PUInt32): HRESULT; stdcall;
    function GetStreamLatency(phnsLatency: PREFERENCE_TIME): HRESULT; stdcall;
    function GetCurrentPadding(pNumPaddingFrames: PUInt32): HRESULT; stdcall;
    function IsFormatSupported(ShareMode: AUDCLNT_SHAREMODE; const pFormat: PWAVEFORMATEX;
      out ppClosestMatch: PWAVEFORMATEX): HRESULT; stdcall;
    function GetMixFormat(out ppDeviceFormat: PWAVEFORMATEX): HRESULT; stdcall;
    function GetDevicePeriod(phnsDefaultDevicePeriod: PREFERENCE_TIME; phnsMinimumDevicePeriod: PREFERENCE_TIME)
      : HRESULT; stdcall;
    function Start(): HRESULT; stdcall;
    function Stop(): HRESULT; stdcall;
    function Reset(): HRESULT; stdcall;
    function SetEventHandle(eventHandle: THandle): HRESULT; stdcall;
    function GetService(const riid: TGUID; out ppv): HRESULT; stdcall;
  end;

  IAudioRenderClient = interface(IUnknown)
    ['{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}']
    function GetBuffer(NumFramesRequested: UInt32; out ppData: PBYTE): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesWritten: UInt32; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IAudioCaptureClient = interface(IUnknown)
    ['{C8ADBD64-E71E-48a0-A4DE-185C395CD317}']
    function GetBuffer(out ppData: PBYTE; pNumFramesToRead: PUInt32; pdwFlags: PDWORD; pu64DevicePosition: PUInt64;
      pu64QPCPosition: PUInt64): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesRead: UInt32): HRESULT; stdcall;
    function GetNextPacketSize(pNumFramesInNextPacket: PUInt32): HRESULT; stdcall;
  end;

implementation

end.
