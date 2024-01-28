unit JAT.NotificationClient;

interface

uses
  System.SysUtils, Winapi.Windows, Winapi.ActiveX,

  JAT.Win.MMDeviceAPI;

type
  TOnDefaultDeviceChanged = procedure(const a_Flow: EDataFlow; const a_Role: ERole; const a_DeviceId: PWideChar)
    of object;

  TNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;
  public
    constructor Create(const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
    destructor Destroy; override;

    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: PWideChar; const key: PROPERTYKEY): HResult; stdcall;
  end;

implementation

{ TNotificationClient }

constructor TNotificationClient.Create(const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
begin
  f_OnDefaultDeviceChanged := a_OnDefaultDeviceChanged; // Store Callback
end;

destructor TNotificationClient.Destroy;
begin
  inherited;
end;

function TNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow; role: ERole;
  pwstrDefaultDeviceId: PWideChar): HResult;
begin
  if Assigned(f_OnDefaultDeviceChanged) then
  begin
    f_OnDefaultDeviceChanged(flow, role, pwstrDefaultDeviceId); // Callback
  end;

  Result := S_OK;
end;

function TNotificationClient.OnDeviceAdded(pwstrDeviceId: PWideChar): HResult;
begin
  Result := S_OK;
end;

function TNotificationClient.OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult;
begin
  Result := S_OK;
end;

function TNotificationClient.OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult;
begin
  Result := S_OK;
end;

function TNotificationClient.OnPropertyValueChanged(pwstrDeviceId: PWideChar; const key: PROPERTYKEY): HResult;
begin
  Result := S_OK;
end;

end.
