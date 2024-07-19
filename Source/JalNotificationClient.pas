unit JalNotificationClient;

interface

uses
  System.SysUtils, Winapi.Windows, Winapi.ActiveX,

  Jal.Win.MMDeviceAPI;

type
  TOnDefaultDeviceChanged = procedure(const a_Flow: EDataFlow; const a_Role: ERole; const a_DeviceId: PWideChar)
    of object;

  TJalNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private
    f_DeviceId: string;
    f_Flow: EDataFlow;
    f_Role: ERole;
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;
  public
    constructor Create(const a_DeviceId: string; const a_Flow: EDataFlow; const a_Role: ERole;
      const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
    destructor Destroy; override;

    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: PWideChar; key: PROPERTYKEY): HResult; stdcall;
  end;

implementation

{ TNotificationClient }

constructor TJalNotificationClient.Create(const a_DeviceId: string; const a_Flow: EDataFlow; const a_Role: ERole;
  const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
begin
  f_DeviceId := a_DeviceId;
  f_Flow := a_Flow;
  f_Role := a_Role;
  f_OnDefaultDeviceChanged := a_OnDefaultDeviceChanged;
end;

destructor TJalNotificationClient.Destroy;
begin
  inherited;
end;

function TJalNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow; role: ERole;
  pwstrDefaultDeviceId: PWideChar): HResult;
begin
  if (Assigned(f_OnDefaultDeviceChanged))  and (flow = f_Flow) and (role = f_Role)
  then
  begin
    f_OnDefaultDeviceChanged(flow, role, pwstrDefaultDeviceId); // Callback
  end;

  Result := S_OK;
end;

function TJalNotificationClient.OnDeviceAdded(pwstrDeviceId: PWideChar): HResult;
begin
  Result := S_OK;
end;

function TJalNotificationClient.OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult;
begin
  Result := S_OK;
end;

function TJalNotificationClient.OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult;
begin
  Result := S_OK;
end;

function TJalNotificationClient.OnPropertyValueChanged(pwstrDeviceId: PWideChar; key: PROPERTYKEY): HResult;
begin
  Result := S_OK;
end;

end.
