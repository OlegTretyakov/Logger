object LogViewFm: TLogViewFm
  Left = 0
  Top = 0
  Width = 1035
  Height = 620
  TabOrder = 0
  object Splitter2: TSplitter
    Left = 0
    Top = 564
    Width = 1035
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 492
    ExplicitWidth = 831
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 157
    Height = 564
    Align = alLeft
    Constraints.MinWidth = 150
    TabOrder = 0
    DesignSize = (
      157
      564)
    object BtnOpen: TButton
      Left = 16
      Top = 8
      Width = 57
      Height = 25
      Caption = #1054#1090#1082#1088#1099#1090#1100
      TabOrder = 0
      OnClick = BtnOpenClick
    end
    object EventsList: TCheckListBox
      Left = 16
      Top = 72
      Width = 125
      Height = 105
      OnClickCheck = EventsListClickCheck
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Style = lbOwnerDrawFixed
      TabOrder = 4
      OnDblClick = EventsListDblClick
      OnDrawItem = EventsListDrawItem
    end
    object EditSearch: TEdit
      Left = 16
      Top = 40
      Width = 104
      Height = 21
      Hint = 'Search (Ctrl+F, F3 for next) '
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object BtnSearch: TButton
      Left = 124
      Top = 38
      Width = 17
      Height = 23
      Hint = 'Search Next (F3)'
      Anchors = [akTop, akRight]
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = BtnSearchClick
    end
    object BtnStats: TButton
      Left = 16
      Top = 531
      Width = 125
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      TabOrder = 5
      OnClick = BtnStatsClick
    end
    object SettingsBtn: TButton
      Left = 75
      Top = 8
      Width = 66
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      TabOrder = 1
      OnClick = SettingsBtnClick
    end
  end
  object MemoBottom: TMemo
    Left = 0
    Top = 568
    Width = 1035
    Height = 52
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object List: TDrawGrid
    Left = 157
    Top = 0
    Width = 878
    Height = 564
    Align = alClient
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 14
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect, goThumbTracking]
    PopupMenu = GridPopupMenu
    TabOrder = 1
    Visible = False
    OnClick = ListClick
    OnDblClick = ListDblClick
    OnDrawCell = ListDrawCell
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.log'
    Filter = 'vLog|*.vlog;*.log;*.txt;*.synlz'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 44
    Top = 432
  end
  object GridPopupMenu: TPopupMenu
    Left = 112
    Top = 432
    object RefreshMnu: TMenuItem
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      OnClick = RefreshMnuClick
    end
    object CompressAndSaveMnuItem: TMenuItem
      Caption = #1057#1078#1072#1090#1100' '#1080' '#1089#1086#1093#1088#1072#1085#1080#1090#1100
      OnClick = CompressAndSaveMnuItemClick
    end
  end
end
