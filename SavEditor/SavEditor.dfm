object Form1: TForm1
  Left = 488
  Top = 156
  Width = 473
  Height = 444
  Caption = 'Save File Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FileTree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 457
    Height = 385
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
    Indent = 19
    PopupMenu = pm1
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnDblClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeItemErase = FileTreeBeforeItemErase
    OnContextPopup = FileTreeContextPopup
    OnCreateEditor = FileTreeCreateEditor
    OnEdited = FileTreeEdited
    OnFreeNode = FileTreeFreeNode
    OnGetText = FileTreeGetText
    OnNodeDblClick = FileTreeNodeDblClick
    OnScroll = FileTreeScroll
    Columns = <
      item
        Position = 0
        Width = 200
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 178
        WideText = 'Value'
      end
      item
        Position = 2
        Width = 75
        WideText = 'Type'
      end>
  end
  object mm1: TMainMenu
    Left = 80
    Top = 32
    object itemFile: TMenuItem
      Caption = 'File'
      object itemOpen: TMenuItem
        Caption = 'Open'
        OnClick = itemOpenClick
      end
      object itemSave: TMenuItem
        Caption = 'Save'
        OnClick = itemSaveClick
      end
    end
    object itemHelp: TMenuItem
      Caption = 'Help'
      object itemAbout: TMenuItem
        Caption = 'About'
        OnClick = itemAboutClick
      end
    end
  end
  object savefile: TSaveDialog
    Left = 208
    Top = 96
  end
  object OpenFile: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 184
    Top = 192
  end
  object OpenSave: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 208
  end
  object pm1: TPopupMenu
    Left = 120
    Top = 136
    object BtnAddHaunter1: TMenuItem
      Caption = 'Add Haunter'
      OnClick = BtnAddHaunter1Click
    end
    object BtnDeleteHaunter1: TMenuItem
      Caption = 'Delete Haunter'
      OnClick = BtnDeleteHaunter1Click
    end
  end
  object OpenXML: TOpenDialog
    Left = 208
    Top = 256
  end
  object OpenImport: TOpenDialog
    Left = 304
    Top = 280
  end
end
