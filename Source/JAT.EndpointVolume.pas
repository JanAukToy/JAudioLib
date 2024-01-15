unit JAT.EndpointVolume;

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
    nChannels: UINT;
    afChannelVolumes: Single;
  end;

  PAUDIO_VOLUME_NOTIFICATION_DATA = ^AUDIO_VOLUME_NOTIFICATION_DATA;

  PIAudioEndpointVolume = ^IAudioEndpointVolume;

  IAudioEndpointVolumeCallback = interface(IUnknown)
    ['{657804FA-D6AD-4496-8A60-352752AF4F89}']
    function OnNotify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;
  end;

  IAudioEndpointVolume = interface(IUnknown)
    ['{5CDF2C82-841E-4546-9722-0CF74078229A}']
    function RegisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;
    function UnregisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;
    function GetChannelCount(out pnChannelCount: UINT): HRESULT; stdcall;
    function SetMasterVolumeLevel(fLevelDB: Single; pguidEventContext: TGUID): HRESULT; stdcall;
    function SetMasterVolumeLevelScalar(fLevelDB: Single; pguidEventContext: TGUID): HRESULT; stdcall;
    function GetMasterVolumeLevel(out fLevelDB: Single): HRESULT; stdcall;
    function GetMasterVolumeLevelScalar(out pfLevel: Single): HRESULT; stdcall;
    function SetChannelVolumeLevel(nChannel: UINT; fLevelDB: Single; pguidEventContext: TGUID): HRESULT; stdcall;
    function SetChannelVolumeLevelScalar(nChannel: UINT; fLevel: Single; pguidEventContext: TGUID): HRESULT; stdcall;
    function GetChannelVolumeLevel(nChannel: Integer; out pfLevelDB: Single): HRESULT; stdcall;
    function GetChannelVolumeLevelScalar(nChannel: UINT; out pfLevel: Single): HRESULT; stdcall;
    function SetMute(bMute: BOOL; pguidEventContext: TGUID): HRESULT; stdcall;
    function GetMute(out bMute: BOOL): HRESULT; stdcall;
    function GetVolumeStepInfo(out pnStep: UINT; out pnStepCount: UINT): HRESULT; stdcall;
    function VolumeStepUp(pguidEventContext: TGUID): HRESULT; stdcall;
    function VolumeStepDown(pguidEventContext: TGUID): HRESULT; stdcall;
    function QueryHardwareSupport(out pdwHardwareSupportMask: DWORD): HRESULT; stdcall;
    function GetVolumeRange(out pflVolumeMindB: Single; out pflVolumeMaxdB: Single; out pflVolumeIncrementdB: Single)
      : HRESULT; stdcall;
  end;

implementation

end.
