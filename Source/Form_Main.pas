unit Form_Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.ComCtrls, Vcl.Controls,
  Vcl.ToolWin, Vcl.Menus, System.Generics.Collections, Vcl.StdCtrls,

  cls_AudioStreamClientThread;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    pgctrl_Device: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    f_AudioStreamClientThread: TAudioStreamClientThread;

    procedure InitVar;
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitVar;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(f_AudioStreamClientThread) then
  begin
    FreeAndNil(f_AudioStreamClientThread);
  end;
end;

procedure TFormMain.InitVar;
begin
  f_AudioStreamClientThread := TAudioStreamClientThread.Create;
end;

end.
