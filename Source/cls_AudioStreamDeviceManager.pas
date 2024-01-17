unit cls_AudioStreamDeviceManager;

interface

uses
  System.SysUtils, System.Classes, System.Win.ComObj, System.SyncObjs,
  System.Generics.Collections, Winapi.Windows, Winapi.ActiveX,

  JAT.MMDeviceAPI, cls_AudioStreamDevice, cls_NotificationClient;

type
  TAudioStreamDeviceManager = class
  private
    // For Audio Streaming Device...
    f_DeviceEnumerator: IMMDeviceEnumerator;
    f_CaptureDevice: TAudioStreamDevice;
    f_RenderDevice: TAudioStreamDevice;

    // For Device Notification...
    f_NotificationClient: TNotificationClient;
    f_OnDefaultDeviceChanged: TOnDefaultDeviceChanged;
  public
    constructor Create(const a_CoInitFlag: Longint);
    destructor Destroy; override;
    procedure Reload;

    property CaptureDevice: TAudioStreamDevice read f_CaptureDevice;
    property RenderDevice: TAudioStreamDevice read f_RenderDevice;
    property OnDefaultDeviceChanged: TOnDefaultDeviceChanged write f_OnDefaultDeviceChanged;
  end;

implementation

{ TAudioStreamDeviceManager }

constructor TAudioStreamDeviceManager.Create(const a_CoInitFlag: Longint);
begin
  // Init COM
  if (Succeeded(CoInitializeEx(nil, a_CoInitFlag))) and
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

destructor TAudioStreamDeviceManager.Destroy;
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

  if Assigned(f_CaptureDevice) then
  begin
    FreeAndNil(f_CaptureDevice);
  end;

  if Assigned(f_RenderDevice) then
  begin
    FreeAndNil(f_RenderDevice);
  end;

  CoUninitialize();

  inherited;
end;

procedure TAudioStreamDeviceManager.Reload;
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
    f_CaptureDevice := TAudioStreamDevice.Create(f_DeviceEnumerator, eCapture);
    f_RenderDevice := TAudioStreamDevice.Create(f_DeviceEnumerator, eRender);
  end;
end;

end.
