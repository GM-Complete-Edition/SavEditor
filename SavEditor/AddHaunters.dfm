object Form2: TForm2
  Left = 847
  Top = 396
  Width = 265
  Height = 163
  Caption = 'Add Haunters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 8
    Top = 8
    Width = 233
    Height = 81
    Caption = 'Select Haunters'
    TabOrder = 0
    object cbb1: TCheckComboBox
      Left = 16
      Top = 24
      Width = 201
      Height = 22
      AutoComplete = False
      ItemHeight = 16
      TabOrder = 0
      Text = 'cbb1'
    end
  end
  object btn1: TButton
    Left = 80
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 168
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 32
    Top = 62
    Width = 75
    Height = 19
    Caption = 'Select All'
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 136
    Top = 62
    Width = 75
    Height = 19
    Caption = 'Deselect All'
    TabOrder = 4
    OnClick = btn4Click
  end
end
