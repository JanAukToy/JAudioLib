unit JalAudioDevice;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys,

  Jal.Win.MMDeviceAPI, Jal.Win.EndpointVolume, JalNotificationClient;

type
  // ***************************************************************************
  // Volume Callback Handler
  TOnChangeVolume = procedure(const a_Data: AUDIO_VOLUME_NOTIFICATION_DATA) of object;

  TAudioEndpointVolumeCallbackHandler = class(TInterfacedObject, IAudioEndpointVolumeCallback)
  private
    f_OnChangeVolume: TOnChangeVolume;
  public
    constructor Create(const a_OnControlChangeNotify: TOnChangeVolume);
    destructor Destroy; override;
    function OnNotify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;
  end;

  // ***************************************************************************
  // Audio Streaming Device Class
  TOnChangeMasterLevel = procedure(const a_Value: Integer) of object;
  TOnChangeMute = procedure(const a_Value: Boolean) of object;

  TJalAudioDevice = class
  private
    f_Ready: Boolean;
    f_VolumeCallbackHandler: TAudioEndpointVolumeCallbackHandler;
    f_PropertyStore: IPropertyStore;

    // Device Emurator...
    f_DeviceEnumerator: IMMDeviceEnumerator;
    f_DataFlow: EDataFlow;
    f_Role: ERole;
    f_NotificationClient: TJalNotificationClient;
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;

    // Device Props...
    f_Device: IMMDevice;
    f_AudioEndpointVolume: IAudioEndpointVolume;
    f_InterfaceFriendlyName: string;
    f_DeviceDesc: string;
    f_FriendlyName: string;
    f_DeviceId: string;
    f_ContainerId: TGUID;

    // Endpoint Volume Props...
    f_ChannelCount: DWORD;
    f_MasterLevel: Single;
    f_Mute: Boolean;
    f_Step: DWORD;
    f_StepCount: DWORD;
    f_Min: Single;
    f_Max: Single;
    f_Spin: Single;

    f_OnChangeMasterLevel: TOnChangeMasterLevel;
    f_OnChangeMute: TOnChangeMute;

    function InitEmurator(const a_CoInitFlag: Integer; const a_DataFlowType: EDataFlow; const a_Role: ERole): Boolean;
    function InitDevice(const a_DataFlowType: EDataFlow; const a_Role: ERole): Boolean;

    procedure SetDeviceDesc(const a_Value: string);
    procedure SetMasterLevel(const a_Value: Single);
    procedure SetMute(const a_Value: Boolean);

    function GetDeviceProps(const a_PropertyStore: IPropertyStore): Boolean;
    function GetAudioEndpointVolumeProps(): Boolean;
    procedure OnControlChangeNotify(const a_Data: AUDIO_VOLUME_NOTIFICATION_DATA);
  public
    constructor Create(const a_CoInitFlag: Longint; const a_DataFlowType: EDataFlow;
      const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged = nil);
    destructor Destroy; override;

    property Ready: Boolean read f_Ready;

    property Device: IMMDevice read f_Device;

    property DeviceDesc: string read f_DeviceDesc write SetDeviceDesc;
    property FriendlyName: string read f_FriendlyName;
    property DeviceId: string read f_DeviceId;
    property ContainerId: TGUID read f_ContainerId;

    property ChannelCount: DWORD read f_ChannelCount;
    property MasterLevel: Single read f_MasterLevel write SetMasterLevel;
    property Mute: Boolean read f_Mute write SetMute;
    property Step: DWORD read f_Step;
    property StepCount: DWORD read f_StepCount;
    property Min: Single read f_Min;
    property Max: Single read f_Max;
    property Spin: Single read f_Spin;

    property OnChangeMasterLevel: TOnChangeMasterLevel write f_OnChangeMasterLevel;
    property OnChangeMute: TOnChangeMute write f_OnChangeMute;
  end;

implementation

{ TAudioEndpointVolumeCallbackHandler }

constructor TAudioEndpointVolumeCallbackHandler.Create(const a_OnControlChangeNotify: TOnChangeVolume);
begin
  inherited Create;

  f_OnChangeVolume := a_OnControlChangeNotify; // Store Callback
end;

destructor TAudioEndpointVolumeCallbackHandler.Destroy;
begin
  inherited;
end;

function TAudioEndpointVolumeCallbackHandler.OnNotify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;
begin
  f_OnChangeVolume(pNotify); // Callback

  Result := S_OK;
end;

{ TAudioStreamDevice }

constructor TJalAudioDevice.Create(const a_CoInitFlag: Longint; const a_DataFlowType: EDataFlow;
  const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged = nil);
begin
  f_DataFlow := a_DataFlowType;
  f_Role := eConsole;
  f_OnDefaultDeviceChanged := a_OnDefaultDeviceChanged;

  // Init Emurator and Init Device
  f_Ready := InitEmurator(a_CoInitFlag, f_DataFlow, f_Role) and InitDevice(f_DataFlow, f_Role);
end;

destructor TJalAudioDevice.Destroy;
begin
  if Assigned(f_VolumeCallbackHandler) then
  begin
    // Unregister Callback Handler
    f_AudioEndpointVolume.UnregisterControlChangeNotify(f_VolumeCallbackHandler);
  end;

  if Assigned(f_NotificationClient) then
  begin
    if f_NotificationClient.RefCount > 0 then
    begin
      // Unregister Notification Client
      f_DeviceEnumerator.UnregisterEndpointNotificationCallback(f_NotificationClient);
    end;

    FreeAndNil(f_NotificationClient);
  end;

  CoUninitialize();

  inherited;
end;

function TJalAudioDevice.InitEmurator(const a_CoInitFlag: Integer; const a_DataFlowType: EDataFlow;
  const a_Role: ERole): Boolean;
begin
  // Init COM
  Result := (Succeeded(CoInitializeEx(nil, a_CoInitFlag))) and // Get DeviceEnumerator
    (Succeeded(CoCreateInstance(CLSID_IMMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator,
    f_DeviceEnumerator)));
end;

function TJalAudioDevice.InitDevice(const a_DataFlowType: EDataFlow; const a_Role: ERole): Boolean;
var
  l_Id: PWideChar;
begin
  Result := False;

  // Get Device
  if Succeeded(f_DeviceEnumerator.GetDefaultAudioEndpoint(a_DataFlowType, a_Role, f_Device)) then
  begin
    // Get Device ID
    if Succeeded(f_Device.GetId(l_Id)) then
    begin
      // Store ID
      f_DeviceId := l_Id;

      f_NotificationClient := TJalNotificationClient.Create(f_DeviceId, a_DataFlowType, a_Role,
        f_OnDefaultDeviceChanged);

      // Register Notification Client
      if Succeeded(f_DeviceEnumerator.RegisterEndpointNotificationCallback(f_NotificationClient)) then
      begin
        // Get Open Property Interface
        if Succeeded(f_Device.OpenPropertyStore(STGM_READWRITE, f_PropertyStore)) then
        begin
          // Get Device Properties
          if GetDeviceProps(f_PropertyStore) then
          begin
            // Get Audio Endpoint Volume
            if Succeeded(f_Device.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, f_AudioEndpointVolume)) then
            begin
              // Create Volume Callback Handler
              f_VolumeCallbackHandler := TAudioEndpointVolumeCallbackHandler.Create(OnControlChangeNotify);

              // Register Volume Callback Handler
              if Succeeded(f_AudioEndpointVolume.RegisterControlChangeNotify(f_VolumeCallbackHandler)) then
              begin
                // Get Audio Endpoint Volume Properties
                Result := GetAudioEndpointVolumeProps;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TJalAudioDevice.SetDeviceDesc(const a_Value: string);
var
  l_Variant: TPropVariant;
begin
  // Cast to Prop Variant
  if Succeeded(InitPropVariantFromString(PChar(a_Value), l_Variant)) and
    Succeeded(f_PropertyStore.SetValue(PKEY_Device_DeviceDesc, l_Variant)) then
  begin
    // Commit Change..
    Succeeded(f_PropertyStore.Commit);
  end;
end;

procedure TJalAudioDevice.SetMasterLevel(const a_Value: Single);
begin
  f_AudioEndpointVolume.SetMasterVolumeLevelScalar(a_Value, nil);
end;

procedure TJalAudioDevice.SetMute(const a_Value: Boolean);
begin
  f_AudioEndpointVolume.SetMute(a_Value, nil);
end;

function TJalAudioDevice.GetDeviceProps(const a_PropertyStore: IPropertyStore): Boolean;
var
  l_PropInterfaceFriendlyName: TPropVariant;
  l_PropDeviceDesc: TPropVariant;
  l_PropFriendlyName: TPropVariant;
  l_PropContainerId: TPropVariant;
begin
  Result := False;

  // Get All Properties
  // [ PKEY_Device_InstanceId ] is IMMDevice::GetId Value
  if (Succeeded(a_PropertyStore.GetValue(PKEY_DeviceInterface_FriendlyName, l_PropInterfaceFriendlyName))) and
    (Succeeded(a_PropertyStore.GetValue(PKEY_Device_DeviceDesc, l_PropDeviceDesc))) and
    (Succeeded(a_PropertyStore.GetValue(PKEY_Device_FriendlyName, l_PropFriendlyName))) and
    (Succeeded(a_PropertyStore.GetValue(PKEY_Device_ContainerId, l_PropContainerId))) then
  begin
    // Store Properties
    f_InterfaceFriendlyName := l_PropInterfaceFriendlyName.pwszVal;
    f_DeviceDesc := l_PropDeviceDesc.pwszVal;
    f_FriendlyName := l_PropFriendlyName.pwszVal;
    f_ContainerId := l_PropContainerId.puuid^;

    Result := True;
  end;
end;

function TJalAudioDevice.GetAudioEndpointVolumeProps: Boolean;
var
  ii: DWORD;
  l_ChannelCount: DWORD;
  l_MasterLevel: Single;
  l_ChannelLevel: Single;
  l_ChannelLevelList: TList<Single>;
  l_Mute: LongBool;
  l_Step: DWORD;
  l_StepCount: DWORD;
  l_Min: Single;
  l_Max: Single;
  l_Spin: Single;
begin
  Result := False;

  // Get All Properties
  if (Succeeded(f_AudioEndpointVolume.GetChannelCount(@l_ChannelCount))) and
    (Succeeded(f_AudioEndpointVolume.GetMasterVolumeLevelScalar(@l_MasterLevel))) and
    (Succeeded(f_AudioEndpointVolume.GetMute(@l_Mute))) and
    (Succeeded(f_AudioEndpointVolume.GetVolumeStepInfo(@l_Step, @l_StepCount))) and
    (Succeeded(f_AudioEndpointVolume.GetVolumeRange(@l_Min, @l_Max, @l_Spin))) then
  begin
    l_ChannelLevelList := TList<Single>.Create;

    try
      // Get All Channel Volume
      for ii := 0 to l_ChannelCount - 1 do
      begin
        if (Succeeded(f_AudioEndpointVolume.GetChannelVolumeLevelScalar(ii, @l_ChannelLevel))) then
        begin
          // Add Value
          l_ChannelLevelList.Add(l_ChannelLevel);
        end;
      end;

      // Check Get All Channel Result
      if l_ChannelCount = DWORD(l_ChannelLevelList.Count) then
      begin
        // Store Properties
        f_ChannelCount := l_ChannelCount;
        f_MasterLevel := l_MasterLevel;
        f_Mute := l_Mute;
        f_Step := l_Step;
        f_StepCount := l_StepCount;
        f_Min := l_Min;
        f_Max := l_Max;
        f_Spin := l_Spin;

        Result := True;
      end;

    finally
      l_ChannelLevelList.Free;
    end;
  end;
end;

procedure TJalAudioDevice.OnControlChangeNotify(const a_Data: AUDIO_VOLUME_NOTIFICATION_DATA);
begin
  f_ChannelCount := a_Data.nChannels;

  if f_MasterLevel <> a_Data.fMasterVolume then
  begin
    f_MasterLevel := a_Data.fMasterVolume;

    if Assigned(f_OnChangeMasterLevel) then
    begin
      f_OnChangeMasterLevel(Round(f_MasterLevel * 100)); // Callback
    end;
  end;

  if f_Mute <> a_Data.bMuted then
  begin
    f_Mute := a_Data.bMuted;

    if Assigned(f_OnChangeMute) then
    begin
      f_OnChangeMute(f_Mute); // Callback
    end;
  end;
end;

end.
