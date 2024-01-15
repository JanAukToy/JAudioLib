unit cls_AudioDeviceManager;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.Generics.Collections, Winapi.Windows, Winapi.ActiveX,

  JAT.MMDeviceAPI, cls_AudioDevice, cls_NotificationClient;

type
  TAudioDeviceManager = class
  private
    // Audio Device...
    f_DeviceEnumerator: IMMDeviceEnumerator;
    f_CaptureDevice: TAudioDevice;
    f_RenderDevice: TAudioDevice;

    // Device Notification...
    f_NotificationClient: TNotificationClient;
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reload;

    property CaptureDevice: TAudioDevice read f_CaptureDevice;
    property RenderDevice: TAudioDevice read f_RenderDevice;
    property OnDefaultDeviceChanged: TOnDefaultDeviceChanged write f_OnDefaultDeviceChanged;
  end;

implementation

{ TAudioDevices }

constructor TAudioDeviceManager.Create;
begin
  // Init COM
  if (Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED))) and
  // Get DeviceEnumerator
    (Succeeded(CoCreateInstance(CLSID_IMMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator,
    f_DeviceEnumerator))) then
  begin
    f_NotificationClient := TNotificationClient.Create(f_OnDefaultDeviceChanged);

    // Register Notification Client
    if Succeeded(f_DeviceEnumerator.RegisterEndpointNotificationCallback(f_NotificationClient)) then
    begin
      // Get Devices
      Reload;
    end;
  end;
end;

destructor TAudioDeviceManager.Destroy;
begin
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

procedure TAudioDeviceManager.Reload;
begin
  if Assigned(f_DeviceEnumerator) then
  begin
    // Destroy Devices
    if Assigned(f_CaptureDevice) then
    begin
      FreeAndNil(f_CaptureDevice);
    end;

    if Assigned(f_RenderDevice) then
    begin
      FreeAndNil(f_RenderDevice);
    end;

    // Get Devices
    f_CaptureDevice := TAudioDevice.Create(f_DeviceEnumerator, eCapture);
    f_RenderDevice := TAudioDevice.Create(f_DeviceEnumerator, eRender);
  end;
end;

end.
