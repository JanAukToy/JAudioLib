object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'JAT Audio Streamer'
  ClientHeight = 461
  ClientWidth = 788
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Meiryo UI'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object pgctrl_Device: TPageControl
    Left = 0
    Top = 0
    Width = 788
    Height = 461
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 16
  end
end
