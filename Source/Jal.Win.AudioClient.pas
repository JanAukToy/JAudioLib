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

  ISimpleAudioVolume = interface(IUnknown)
    ['{87CE5498-68D6-44E5-9215-6DA47EF883D8}']
    function SetMasterVolume(fLevel: Single; EventContext: TGUID): HRESULT; stdcall;
    function GetMasterVolume(out pfLevel: Single): HRESULT; stdcall;
    function SetMute(const bMute: BOOL; EventContext: TGUID): HRESULT; stdcall;
    function GetMute(out pbMute: BOOL): HRESULT; stdcall;
  end;

  IAudioClient = interface(IUnknown)
    ['{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}']
    function Initialize(ShareMode: _AUDCLNT_SHAREMODE; StreamFlags: DWORD; hnsBufferDuration: REFERENCE_TIME;
      hnsPeriodicity: REFERENCE_TIME; const pFormat: PWAVEFORMATEX; AudioSessionGuid: TGUID): HRESULT; stdcall;
    function GetBufferSize(out pNumBufferFrames: UINT32): HRESULT; stdcall;
    function GetStreamLatency(out phnsLatency: REFERENCE_TIME): HRESULT; stdcall;
    function GetCurrentPadding(out pNumPaddingFrames: UINT32): HRESULT; stdcall;
    function IsFormatSupported(ShareMode: AUDCLNT_SHAREMODE; const pFormat: PWAVEFORMATEX;
      out ppClosestMatch: PWAVEFORMATEX): HRESULT; stdcall;
    function GetMixFormat(out ppDeviceFormat: PWAVEFORMATEX): HRESULT; stdcall;
    function GetDevicePeriod(out phnsDefaultDevicePeriod: REFERENCE_TIME; out phnsMinimumDevicePeriod: REFERENCE_TIME)
      : HRESULT; stdcall;
    function Start(): HRESULT; stdcall;
    function Stop(): HRESULT; stdcall;
    function Reset(): HRESULT; stdcall;
    function SetEventHandle(eventHandle: THandle): HRESULT; stdcall;
    function GetService(const riid: TGUID; out ppv: Pointer): HRESULT; stdcall;
  end;

  IAudioRenderClient = interface(IUnknown)
    ['{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}']
    function GetBuffer(NumFramesRequested: UINT32; out ppData: PBYTE): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesWritten: UINT32; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IAudioCaptureClient = interface(IUnknown)
    ['{C8ADBD64-E71E-48a0-A4DE-185C395CD317}']
    function GetBuffer(out ppData: PBYTE; out pNumFramesToRead: UINT32; out pdwFlags: DWORD;
      out pu64DevicePosition: UINT64; out pu64QPCPosition: UINT64): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesRead: UINT32): HRESULT; stdcall;
    function GetNextPacketSize(out pNumFramesInNextPacket: UINT32): HRESULT; stdcall;
  end;

implementation

end.
