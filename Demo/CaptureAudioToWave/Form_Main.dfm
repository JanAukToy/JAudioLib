object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'JAudioLib - CaptureAudioToWave'
  ClientHeight = 211
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MaxHeight = 250
  Constraints.MaxWidth = 350
  Constraints.MinHeight = 250
  Constraints.MinWidth = 350
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
    Width = 334
    Height = 161
    Align = alClient
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 0
    object pnl_SamplingRate: TPanel
      Left = 11
      Top = 36
      Width = 312
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
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
        Width = 182
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
      end
    end
    object pnl_Bits: TPanel
      Left = 11
      Top = 61
      Width = 312
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
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
        Width = 182
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
      end
    end
    object pnl_Channel: TPanel
      Left = 11
      Top = 86
      Width = 312
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
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
        Width = 182
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Monoral'
        Items.Strings = (
          'Monoral'
          'Stereo')
      end
    end
    object pnl_AudioType: TPanel
      Left = 11
      Top = 11
      Width = 312
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
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
        Width = 182
        Height = 23
        Align = alClient
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Mic'
        Items.Strings = (
          'Mic'
          'System')
      end
    end
  end
  object pnl_Control: TPanel
    Left = 0
    Top = 161
    Width = 334
    Height = 50
    Align = alBottom
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
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
      Left = 223
      Top = 11
      Width = 100
      Height = 28
      Align = alRight
      Caption = 'END'
      TabOrder = 1
      OnClick = btn_EndCaptureClick
    end
  end
end
