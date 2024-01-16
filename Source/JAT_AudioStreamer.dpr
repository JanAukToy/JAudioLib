program JAT_AudioStreamer;

uses
  Vcl.Forms,
  Form_Main in 'Form_Main.pas' {FormMain},
  JAT.MMDeviceAPI in 'JAT.MMDeviceAPI.pas',
  JAT.AudioClient in 'JAT.AudioClient.pas',
  JAT.EndpointVolume in 'JAT.EndpointVolume.pas',
  cls_NotificationClient in 'cls_NotificationClient.pas',
  cls_AudioStreamClientThread in 'cls_AudioStreamClientThread.pas',
  cls_AudioStreamDevice in 'cls_AudioStreamDevice.pas',
  cls_AudioStreamDeviceManager in 'cls_AudioStreamDeviceManager.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
