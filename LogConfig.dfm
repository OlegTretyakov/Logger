object LogConfigFrm: TLogConfigFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 188
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 124
    Top = 115
    Width = 76
    Height = 39
    Alignment = taCenter
    Caption = #1040#1088#1093#1080#1074#1080#1088#1086#1074#1072#1090#1100' '#1087#1077#1088#1077#1076' '#1091#1076#1072#1083#1077#1085#1080#1077#1084
    WordWrap = True
  end
  object Label2: TLabel
    Left = 128
    Top = 89
    Width = 74
    Height = 13
    Alignment = taCenter
    Caption = #1061#1088#1072#1085#1080#1090#1100', '#1076#1085#1077#1081
    WordWrap = True
  end
  object Label3: TLabel
    Left = 128
    Top = 9
    Width = 77
    Height = 26
    Alignment = taCenter
    Caption = #1057#1073#1088#1086#1089' '#1082#1072#1078#1076#1099#1077' N '#1084#1080#1085'.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 128
    Top = 45
    Width = 70
    Height = 26
    Alignment = taCenter
    Caption = #1052#1072#1082#1089'. '#1088#1072#1079#1084#1077#1088' '#1092#1072#1081#1083#1072', '#1052#1041
    WordWrap = True
  end
  object EventsList: TCheckListBox
    Left = 0
    Top = 0
    Width = 122
    Height = 188
    Align = alLeft
    ItemHeight = 13
    Style = lbOwnerDrawFixed
    TabOrder = 0
    OnDrawItem = EventsListDrawItem
  end
  object CheckBox1: TCheckBox
    Left = 211
    Top = 122
    Width = 17
    Height = 19
    Alignment = taLeftJustify
    TabOrder = 4
  end
  object ApplyBtn: TBitBtn
    Left = 146
    Top = 155
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
    TabOrder = 5
    OnClick = ApplyBtnClick
  end
  object AutoFlushTimeOutEdit: TJvValidateEdit
    Left = 211
    Top = 14
    Width = 30
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 100.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 1
  end
  object MaxMBSizeEdit: TJvValidateEdit
    Left = 211
    Top = 50
    Width = 30
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 250.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 2
  end
  object ArchiveAfterDaysEdit: TJvValidateEdit
    Left = 211
    Top = 86
    Width = 30
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    EditText = '7'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 2
    MaxValue = 30.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 3
  end
end
