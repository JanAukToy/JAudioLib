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
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;
  public
    constructor Create(const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
    destructor Destroy; override;

    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: PWideChar; key: PROPERTYKEY): HResult; stdcall;
  end;

implementation

{ TNotificationClient }

constructor TJalNotificationClient.Create(const a_OnDefaultDeviceChanged: TOnDefaultDeviceChanged);
begin
  f_OnDefaultDeviceChanged := a_OnDefaultDeviceChanged; // Store Callback
end;

destructor TJalNotificationClient.Destroy;
begin
  inherited;
end;

function TJalNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow; role: ERole;
  pwstrDefaultDeviceId: PWideChar): HResult;
begin
  if Assigned(f_OnDefaultDeviceChanged) then
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
