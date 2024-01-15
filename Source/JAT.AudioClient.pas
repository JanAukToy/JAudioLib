unit JAT.AudioClient;

interface

uses
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys;

const
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}';

type
  _AUDCLNT_SHAREMODE = (AUDCLNT_SHAREMODE_SHARED, AUDCLNT_SHAREMODE_EXCLUSIVE);
  AUDCLNT_SHAREMODE = _AUDCLNT_SHAREMODE;

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
      hnsPeriodicity: REFERENCE_TIME; const pFormat: WAVEFORMATEX; AudioSessionGuid: TGUID): HRESULT; stdcall;
    function GetBufferSize(out pNumBufferFrames: UINT32): HRESULT; stdcall;
    function GetStreamLatency(out phnsLatency: REFERENCE_TIME): HRESULT; stdcall;
    function GetCurrentPadding(out pNumPaddingFrames: UINT32): HRESULT; stdcall;
    function IsFormatSupported(ShareMode: AUDCLNT_SHAREMODE; const pFormat: WAVEFORMATEX;
      out ppClosestMatch: WAVEFORMATEX): HRESULT; stdcall;
    function GetMixFormat(out ppDeviceFormat: WAVEFORMATEX): HRESULT; stdcall;
    function GetDevicePeriod(out phnsDefaultDevicePeriod: REFERENCE_TIME; out phnsMinimumDevicePeriod: REFERENCE_TIME)
      : HRESULT; stdcall;
    function Start(): HRESULT; stdcall;
    function Stop(): HRESULT; stdcall;
    function Reset(): HRESULT; stdcall;
    function SetEventHandle(eventHandle: THandle): HRESULT; stdcall;
    function GetService(const riid: TGUID; out ppv: Pointer): HRESULT; stdcall;
  end;

implementation

end.
