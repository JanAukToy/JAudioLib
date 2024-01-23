object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'JAT Audio Streamer'
  ClientHeight = 147
  ClientWidth = 288
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Meiryo UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object pnl_Settings: TPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 97
    Align = alClient
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    ExplicitWidth = 488
    object pnl_SamplingRate: TPanel
      Left = 11
      Top = 11
      Width = 266
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 304
      ExplicitTop = 56
      ExplicitWidth = 185
      object txt_SamplingRate: TLabel
        Left = 0
        Top = 0
        Width = 100
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'SamplingRate:'
        Layout = tlCenter
      end
      object cmb_SamplingRate: TComboBox
        Left = 100
        Top = 0
        Width = 166
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = '8000'
        Items.Strings = (
          '8000'
          '16000'
          '44100'
          '48000')
        ExplicitLeft = 86
        ExplicitWidth = 99
      end
    end
    object pnl_Bits: TPanel
      Left = 11
      Top = 36
      Width = 266
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 312
      ExplicitTop = 64
      ExplicitWidth = 185
      object txt_Bits: TLabel
        Left = 0
        Top = 0
        Width = 100
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'Bits:'
        Layout = tlCenter
      end
      object cmb_Bits: TComboBox
        Left = 100
        Top = 0
        Width = 166
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = '8'
        Items.Strings = (
          '8'
          '16'
          '32')
        ExplicitLeft = 86
        ExplicitWidth = 99
      end
    end
    object pnl_Channel: TPanel
      Left = 11
      Top = 61
      Width = 266
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 1000
      ExplicitWidth = 466
      object txt_Channel: TLabel
        Left = 0
        Top = 0
        Width = 100
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'Channel:'
        Layout = tlCenter
        ExplicitTop = 24
      end
      object cmb_Channel: TComboBox
        Left = 100
        Top = 0
        Width = 166
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Monoral'
        Items.Strings = (
          'Monoral'
          'Stereo')
        ExplicitLeft = 86
        ExplicitWidth = 99
      end
    end
  end
  object pnl_Control: TPanel
    Left = 0
    Top = 97
    Width = 288
    Height = 50
    Align = alBottom
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    ExplicitTop = 112
    object btn_StartCapture: TButton
      Left = 11
      Top = 11
      Width = 100
      Height = 28
      Align = alLeft
      Caption = 'START'
      TabOrder = 0
      OnClick = btn_StartCaptureClick
      ExplicitLeft = 202
    end
    object btn_EndCapture: TButton
      Left = 177
      Top = 11
      Width = 100
      Height = 28
      Align = alRight
      Caption = 'END'
      TabOrder = 1
      OnClick = btn_EndCaptureClick
      ExplicitLeft = 19
      ExplicitTop = 19
    end
  end
end
