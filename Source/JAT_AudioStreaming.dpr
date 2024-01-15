program JAT_AudioStreaming;

uses
  Vcl.Forms,
  Form_Main in 'Form_Main.pas' {FormMain},
  cls_AudioDeviceManager in 'cls_AudioDeviceManager.pas',
  JAT.MMDeviceAPI in 'JAT.MMDeviceAPI.pas',
  JAT.AudioClient in 'JAT.AudioClient.pas',
  JAT.EndpointVolume in 'JAT.EndpointVolume.pas',
  cls_AudioDevice in 'cls_AudioDevice.pas',
  cls_NotificationClient in 'cls_NotificationClient.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
