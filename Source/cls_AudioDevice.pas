unit cls_AudioDevice;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Winapi.Windows, Winapi.ActiveX, Winapi.PropSys,

  JAT.MMDeviceAPI, JAT.EndpointVolume;

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
  // Audio Device Class
  TOnChangeMasterLevel = procedure(const a_Value: Integer) of object;
  TOnChangeMute = procedure(const a_Value: Boolean) of object;

  TAudioDevice = class
  private
    f_Ready: Boolean;

    f_VolumeCallbackHandler: TAudioEndpointVolumeCallbackHandler;
    f_PropertyStore: IPropertyStore;

    f_Device: IMMDevice;
    f_AudioEndpointVolume: IAudioEndpointVolume;
    f_InterfaceFriendlyName: string;
    f_DeviceDesc: string;
    f_FriendlyName: string;
    f_InstanceId: string;
    f_ContainerId: TGUID;

    f_ChannelCount: Cardinal;
    f_MasterLevel: Single;
    f_Mute: Boolean;
    f_Step: Cardinal;
    f_StepCount: Cardinal;
    f_Min: Single;
    f_Max: Single;
    f_Spin: Single;

    f_OnChangeMasterLevel: TOnChangeMasterLevel;
    f_OnChangeMute: TOnChangeMute;

    procedure SetDeviceDesc(const a_Value: string);
    procedure SetMasterLevel(const a_Value: Single);
    procedure SetMute(const a_Value: Boolean);

    function GetDeviceProps(const a_PropertyStore: IPropertyStore): Boolean;
    function GetAudioEndpointVolumeProps(): Boolean;
    procedure OnControlChangeNotify(const a_Data: AUDIO_VOLUME_NOTIFICATION_DATA);
  public
    constructor Create(const a_Enumerator: IMMDeviceEnumerator; const a_DataFlowType: EDataFlow);
    destructor Destroy; override;

    property Ready: Boolean read f_Ready;

    property InterfaceFriendlyName: string read f_InterfaceFriendlyName;
    property DeviceDesc: string read f_DeviceDesc write SetDeviceDesc;
    property FriendlyName: string read f_FriendlyName;
    property InstanceId: string read f_InstanceId;
    property ContainerId: TGUID read f_ContainerId;

    property ChannelCount: Cardinal read f_ChannelCount;
    property MasterLevel: Single read f_MasterLevel write SetMasterLevel;
    property Mute: Boolean read f_Mute write SetMute;
    property Step: Cardinal read f_Step;
    property StepCount: Cardinal read f_StepCount;
    property Min: Single read f_Min;
    property Max: Single read f_Max;
    property Spin: Single read f_Spin;

    property OnChangeMasterLevel: TOnChangeMasterLevel write f_OnChangeMasterLevel;
    property OnChangeMute: TOnChangeMute write f_OnChangeMute;
  end;

implementation

{ TIAudioEndpointVolumeCallback }

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

{ TAudioDevice }

constructor TAudioDevice.Create(const a_Enumerator: IMMDeviceEnumerator; const a_DataFlowType: EDataFlow);
var
  l_Id: PWideChar;
  l_PointAudioEndpointVolume: Pointer;
begin
  f_Ready := False;

  // Get Device
  if Succeeded(a_Enumerator.GetDefaultAudioEndpoint(a_DataFlowType, eConsole, f_Device)) then
  begin
    // Get Device ID
    if Succeeded(f_Device.GetId(l_Id)) then
    begin
      // Store ID
      f_InstanceId := l_Id;

      // Get Open Property Interface
      if Succeeded(f_Device.OpenPropertyStore(STGM_READWRITE, f_PropertyStore)) then
      begin
        // Get Device Properties
        if GetDeviceProps(f_PropertyStore) then
        begin
          // Get Audio Endpoint Volume Pointer Interface
          if Succeeded(f_Device.Activate(IID_IAudioEndpointVolume, CLSCTX_INPROC_SERVER, nil,
            l_PointAudioEndpointVolume)) then
          begin
            // Cast to Audio Endpoint Volume
            f_AudioEndpointVolume := IAudioEndpointVolume(l_PointAudioEndpointVolume) as IAudioEndpointVolume;

            // Create Volume Callback Handler
            f_VolumeCallbackHandler := TAudioEndpointVolumeCallbackHandler.Create(OnControlChangeNotify);

            // Register Volume Callback Handler
            if Succeeded(f_AudioEndpointVolume.RegisterControlChangeNotify(f_VolumeCallbackHandler)) then
            begin
              // Get Audio Endpoint Volume Properties
              f_Ready := GetAudioEndpointVolumeProps;
            end;
          end;
        end;
      end;
    end;
  end;
end;

destructor TAudioDevice.Destroy;
begin
  if Assigned(f_VolumeCallbackHandler) then
  begin
    // Unregister Callback Handler
    f_AudioEndpointVolume.UnregisterControlChangeNotify(f_VolumeCallbackHandler);
  end;

  inherited;
end;

procedure TAudioDevice.SetDeviceDesc(const a_Value: string);
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

procedure TAudioDevice.SetMasterLevel(const a_Value: Single);
begin
  f_AudioEndpointVolume.SetMasterVolumeLevelScalar(a_Value, GUID_NULL);
end;

procedure TAudioDevice.SetMute(const a_Value: Boolean);
begin
  f_AudioEndpointVolume.SetMute(a_Value, GUID_NULL);
end;

function TAudioDevice.GetDeviceProps(const a_PropertyStore: IPropertyStore): Boolean;
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

function TAudioDevice.GetAudioEndpointVolumeProps: Boolean;
var
  ii: Cardinal;
  l_ChannelCount: Cardinal;
  l_MasterLevel: Single;
  l_ChannelLevel: Single;
  l_ChannelLevelList: TList<Single>;
  l_Mute: LongBool;
  l_Step: Cardinal;
  l_StepCount: Cardinal;
  l_Min: Single;
  l_Max: Single;
  l_Spin: Single;
begin
  Result := False;

  // Get All Properties
  if (Succeeded(f_AudioEndpointVolume.GetChannelCount(l_ChannelCount))) and
    (Succeeded(f_AudioEndpointVolume.GetMasterVolumeLevelScalar(l_MasterLevel))) and
    (Succeeded(f_AudioEndpointVolume.GetMute(l_Mute))) and
    (Succeeded(f_AudioEndpointVolume.GetVolumeStepInfo(l_Step, l_StepCount))) and
    (Succeeded(f_AudioEndpointVolume.GetVolumeRange(l_Min, l_Max, l_Spin))) then
  begin
    l_ChannelLevelList := TList<Single>.Create;

    try
      // Get All Channel Volume
      for ii := 0 to l_ChannelCount - 1 do
      begin
        if (Succeeded(f_AudioEndpointVolume.GetChannelVolumeLevelScalar(ii, l_ChannelLevel))) then
        begin
          // Add Value
          l_ChannelLevelList.Add(l_ChannelLevel);
        end;
      end;

      // Check Get All Channel Result
      if l_ChannelCount = Cardinal(l_ChannelLevelList.Count) then
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

procedure TAudioDevice.OnControlChangeNotify(const a_Data: AUDIO_VOLUME_NOTIFICATION_DATA);
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
