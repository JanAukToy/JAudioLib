unit JAT.MMDeviceAPI;

interface

uses
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys,
  JAT.EndpointVolume, JAT.AudioClient;

const
  CLSID_IMMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IMMNotificationClient: TGUID = '{7991EEC9-7E89-4D85-8390-6C703CEC60C0}';
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';

  DEVICE_STATE_ACTIVE: DWORD = $00000001;
  DEVICE_STATE_DISABLED: DWORD = $00000002;
  DEVICE_STATE_NOTPRESENT: DWORD = $00000004;
  DEVICE_STATE_UNPLUGGED: DWORD = $00000008;
  DEVICE_STATEMASK_ALL: DWORD = $0000000F;

  AUDCLNT_STREAMFLAGS_CROSSPROCESS = $00010000;
  AUDCLNT_STREAMFLAGS_LOOPBACK = $00020000;
  AUDCLNT_STREAMFLAGS_EVENTCALLBACK = $00040000;
  AUDCLNT_STREAMFLAGS_NOPERSIST = $00080000;
  AUDCLNT_STREAMFLAGS_RATEADJUST = $00100000;
  AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM = $80000000;
  AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY = $08000000;

  PKEY_DeviceInterface_FriendlyName: TPropertyKey = (fmtid: (D1: $026E516E; D2: $B814; D3: $414B;
    D4: ($83, $CD, $85, $6D, $6F, $EF, $48, $22)); pid: 2);

  PKEY_Device_DeviceDesc: TPropertyKey = (fmtid: (D1: $A45C254E; D2: $DF1C; D3: $4EFD;
    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0)); pid: 2);

  PKEY_Device_InstanceId: TPropertyKey = (fmtid: (D1: $78C34FC8; D2: $104A; D3: $4ACA;
    D4: ($9E, $A4, $52, $4D, $52, $99, $6E, $57)); pid: 256);

  PKEY_Device_FriendlyName: TPropertyKey = (fmtid: (D1: $A45C254E; D2: $DF1C; D3: $4EFD;
    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0)); pid: 14);

  PKEY_Device_ContainerId: TPropertyKey = (fmtid: (D1: $8C7ED206; D2: $3F8A; D3: $4827;
    D4: ($B3, $AB, $AE, $9E, $1F, $AE, $FC, $6C)); pid: 2);

type
  EDataFlow = (eRender = $00000000, eCapture = $00000001, eAll = $00000002, EDataFlow_enum_count = $00000003);
  ERole = (eConsole = $00000000, eMultimedia = $00000001, eCommunications = $00000002, ERole_enum_count = $00000003);

  PPROPVARIANT = ^PROPVARIANT;
  PIMMDevice = ^IMMDevice;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: PPROPVARIANT; out ppInterface: Pointer)
      : HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out ppProperties: IPropertyStore): HRESULT; stdcall;
    function GetId(out ppstrId: PWideChar): HRESULT; stdcall;
    function GetState(out pdwState: DWORD): HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out pcDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out ppDevice: IMMDevice): HRESULT; stdcall;
  end;

  IMMNotificationClient = interface(IUnknown)
    ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']
    function OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HRESULT; stdcall;
    function OnDeviceAdded(pwstrDeviceId: PWideChar): HRESULT; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: PWideChar): HRESULT; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: PWideChar): HRESULT; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: PWideChar; const key: PROPERTYKEY): HRESULT; stdcall;
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow; dwStateMask: DWORD; out ppDevices: IMMDeviceCollection)
      : HRESULT; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: EDataFlow; role: ERole; out ppEndpoint: IMMDevice): HRESULT; stdcall;
    function GetDevice(pwstrId: PWideChar; out ppDevice: IMMDevice): HRESULT; stdcall;
    function RegisterEndpointNotificationCallback(pClient: IMMNotificationClient): HRESULT; stdcall;
    function UnregisterEndpointNotificationCallback(pClient: IMMNotificationClient): HRESULT; stdcall;
  end;

implementation

end.
