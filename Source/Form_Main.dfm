object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'JAT Audio Streamer'
  ClientHeight = 218
  ClientWidth = 268
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
    Width = 268
    Height = 168
    Align = alClient
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    ExplicitWidth = 284
    ExplicitHeight = 96
    object pnl_SamplingRate: TPanel
      Left = 11
      Top = 36
      Width = 246
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 11
      ExplicitWidth = 262
      object txt_SamplingRate: TLabel
        Left = 0
        Top = 0
        Width = 130
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'SamplingRate (kHz):'
        Layout = tlCenter
      end
      object cmb_SamplingRate: TComboBox
        Left = 130
        Top = 0
        Width = 116
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
        ExplicitLeft = 100
        ExplicitWidth = 162
      end
    end
    object pnl_Bits: TPanel
      Left = 11
      Top = 61
      Width = 246
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 36
      ExplicitWidth = 262
      object txt_Bits: TLabel
        Left = 0
        Top = 0
        Width = 130
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'Bits:'
        Layout = tlCenter
      end
      object cmb_Bits: TComboBox
        Left = 130
        Top = 0
        Width = 116
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
        ExplicitLeft = 100
        ExplicitWidth = 162
      end
    end
    object pnl_Channel: TPanel
      Left = 11
      Top = 86
      Width = 246
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitTop = 61
      ExplicitWidth = 262
      object txt_Channel: TLabel
        Left = 0
        Top = 0
        Width = 130
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'Channel:'
        Layout = tlCenter
      end
      object cmb_Channel: TComboBox
        Left = 130
        Top = 0
        Width = 116
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Monoral'
        Items.Strings = (
          'Monoral'
          'Stereo')
        ExplicitLeft = 100
        ExplicitWidth = 162
      end
    end
    object pnl_AudioType: TPanel
      Left = 11
      Top = 11
      Width = 246
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 0
      object txt_AudioType: TLabel
        Left = 0
        Top = 0
        Width = 130
        Height = 25
        Align = alLeft
        AutoSize = False
        Caption = 'AudioType:'
        Layout = tlCenter
      end
      object cmb_AudioType: TComboBox
        Left = 130
        Top = 0
        Width = 116
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Mic'
        Items.Strings = (
          'Mic'
          'System')
        ExplicitLeft = 100
        ExplicitWidth = 162
      end
    end
  end
  object pnl_Control: TPanel
    Left = 0
    Top = 168
    Width = 268
    Height = 50
    Align = alBottom
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    ExplicitTop = 96
    ExplicitWidth = 284
    object btn_StartCapture: TButton
      Left = 11
      Top = 11
      Width = 100
      Height = 28
      Align = alLeft
      Caption = 'START'
      TabOrder = 0
      OnClick = btn_StartCaptureClick
    end
    object btn_EndCapture: TButton
      Left = 157
      Top = 11
      Width = 100
      Height = 28
      Align = alRight
      Caption = 'END'
      TabOrder = 1
      OnClick = btn_EndCaptureClick
      ExplicitLeft = 173
    end
  end
end
