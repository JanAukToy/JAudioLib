unit Jal.Win.EndpointVolume;

interface

uses
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys;

const
  IID_IAudioEndpointVolume: TGUID = '{5CDF2C82-841E-4546-9722-0CF74078229A}';

type
  AUDIO_VOLUME_NOTIFICATION_DATA = record
    guidEventContext: TGUID;
    bMuted: BOOL;
    fMasterVolume: Single;
    nChannels: DWORD;
    afChannelVolumes: Single;
  end;

  PAUDIO_VOLUME_NOTIFICATION_DATA = ^AUDIO_VOLUME_NOTIFICATION_DATA;

  IAudioEndpointVolumeCallback = interface(IUnknown)
    ['{657804FA-D6AD-4496-8A60-352752AF4F89}']
    function OnNotify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;
  end;

  IAudioEndpointVolume = interface(IUnknown)
    ['{5CDF2C82-841E-4546-9722-0CF74078229A}']
    function RegisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;
    function UnregisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;
    function GetChannelCount(pnChannelCount: PDWORD): HRESULT; stdcall;
    function SetMasterVolumeLevel(fLevelDB: Single; pguidEventContext: PGUID): HRESULT; stdcall;
    function SetMasterVolumeLevelScalar(fLevelDB: Single; pguidEventContext: PGUID): HRESULT; stdcall;
    function GetMasterVolumeLevel(out fLevelDB: Single): HRESULT; stdcall;
    function GetMasterVolumeLevelScalar(pfLevel: PSingle): HRESULT; stdcall;
    function SetChannelVolumeLevel(nChannel: DWORD; fLevelDB: Single; pguidEventContext: PGUID): HRESULT; stdcall;
    function SetChannelVolumeLevelScalar(nChannel: DWORD; fLevel: Single; pguidEventContext: PGUID): HRESULT; stdcall;
    function GetChannelVolumeLevel(nChannel: Integer; pfLevelDB: PSingle): HRESULT; stdcall;
    function GetChannelVolumeLevelScalar(nChannel: DWORD; pfLevel: PSingle): HRESULT; stdcall;
    function SetMute(bMute: BOOL; pguidEventContext: PGUID): HRESULT; stdcall;
    function GetMute(bMute: PBOOL): HRESULT; stdcall;
    function GetVolumeStepInfo(pnStep: PDWORD; pnStepCount: PDWORD): HRESULT; stdcall;
    function VolumeStepUp(pguidEventContext: PGUID): HRESULT; stdcall;
    function VolumeStepDown(pguidEventContext: PGUID): HRESULT; stdcall;
    function QueryHardwareSupport(pdwHardwareSupportMask: PDWORD): HRESULT; stdcall;
    function GetVolumeRange(pflVolumeMindB: PSingle; pflVolumeMaxdB: PSingle; pflVolumeIncrementdB: PSingle)
      : HRESULT; stdcall;
  end;

implementation

end.
