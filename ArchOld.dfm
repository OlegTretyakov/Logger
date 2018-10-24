object ArchOldLogsFrm: TArchOldLogsFrm
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = #1040#1088#1093#1080#1074#1072#1094#1080#1103' '#1078#1091#1088#1085#1072#1083#1072' '#1089#1086#1073#1099#1090#1080#1081
  ClientHeight = 151
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 41
    Width = 350
    Height = 13
    AutoSize = False
  end
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 350
    Height = 13
    AutoSize = False
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 18
    Width = 350
    Height = 20
    TabOrder = 0
  end
  object ProgressBar2: TProgressBar
    Left = 8
    Top = 56
    Width = 350
    Height = 20
    TabOrder = 1
  end
  object ProgressBar3: TProgressBar
    Left = 8
    Top = 96
    Width = 350
    Height = 20
    TabOrder = 2
  end
  object CancelBtn: TBitBtn
    Left = 142
    Top = 122
    Width = 81
    Height = 29
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 3
    OnClick = CancelBtnClick
  end
end
