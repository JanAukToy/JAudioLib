program PlayAudioFromWave;

uses
  Vcl.Forms,
  form_Main in 'form_Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
