object SelectUnArchItemFrm: TSelectUnArchItemFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 249
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object UnArchItemsGrid: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 480
    Height = 249
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toReportMode, toWheelPanning, toVariableNodeHeight]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    OnGetText = UnArchItemsGridGetText
    OnNodeDblClick = UnArchItemsGridNodeDblClick
    Columns = <
      item
        CaptionAlignment = taCenter
        CheckType = ctNone
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
        Position = 0
        Width = 360
        WideText = #1055#1091#1090#1100
      end
      item
        Alignment = taCenter
        CaptionAlignment = taCenter
        CheckType = ctNone
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
        Position = 1
        Width = 120
        WideText = #1056#1072#1079#1084#1077#1088
      end>
  end
end
