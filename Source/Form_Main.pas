unit Form_Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.ComCtrls, Vcl.Controls,
  Vcl.ToolWin, Vcl.Menus, System.Generics.Collections, Vcl.StdCtrls,

  cls_AudioDeviceManager;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    pgctrl_Device: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    f_AudioDeviceManager: TAudioDeviceManager;

    procedure InitVar;
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

uses
  cls_AudioDevice;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitVar;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(f_AudioDeviceManager);
end;

procedure TFormMain.InitVar;
begin
  // Create Device LIst
  f_AudioDeviceManager := TAudioDeviceManager.Create;
end;

end.
