unit SavEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, VirtualTrees, SavClass, ShlObj , ActiveX, Editors,
  ExtCtrls,StdCtrls,Registry;

type
  TForm1 = class(TForm)
    mm1: TMainMenu;
    itemFile: TMenuItem;
    itemHelp: TMenuItem;
    itemAbout: TMenuItem;
    itemOpen: TMenuItem;
    itemSave: TMenuItem;
    savefile: TSaveDialog;
    OpenFile: TOpenDialog;
    FileTree: TVirtualStringTree;
    OpenSave: TOpenDialog;
    pm1: TPopupMenu;
    BtnAddHaunter1: TMenuItem;
    BtnDeleteHaunter1: TMenuItem;
    OpenXML: TOpenDialog;
    OpenImport: TOpenDialog;
    procedure itemOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileTreeBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure FileTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure itemSaveClick(Sender: TObject);
    function GetDirectoryPath(Folder:Integer): string;
    procedure FileTreeCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure FileTreeScroll(Sender: TBaseVirtualTree; DeltaX,
      DeltaY: Integer);
    procedure FileTreeNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure FileTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FileTreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FileTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure BtnAddHaunter1Click(Sender: TObject);
    procedure BtnDeleteHaunter1Click(Sender: TObject);
    procedure itemAboutClick(Sender: TObject);
    function MyMessageDlg(const Msg: string; DlgTypt: TmsgDlgType; button: TMsgDlgButtons; Caption: array of string; dlgcaption: string): Integer;
    function GetPath(): string;
    procedure SetPath(path: string);
  private
    { Private declarations }
  public
    cSavFile: TSavFile;
    { Public declarations }
  end;

  const
    COMMON_DOCUMENTS = 46;
    SAVEDATA_PATH = '\Ghost Master\SaveGames\';
    VERSION = '1.1';
    NEWLINE = #13#10;

var
  Form1: TForm1;

implementation

uses AddHaunters;

{$R *.dfm}

procedure TForm1.itemOpenClick(Sender: TObject);
var
  ANode,ANode2: PVirtualNode;
  Data: PPropertyData;
  cPath: string;

begin
  Filetree.Clear;
  cSavFile.Free;
  FileTree.NodeDataSize:= SizeOf(TPropertyData);
  cSavFile:= TSavFile.Create(FileTree);
  OpenSave.InitialDir:= GetDirectoryPath(COMMON_DOCUMENTS) + SAVEDATA_PATH;
  OpenSave.Filter:= 'Ghost Master Save File|*.sav';
  if OpenSave.Execute then
  begin
      cSavFile.filename:= extractfilename(OpenSave.FileName);
      cSavFile.setMemoryStream(OpenSave.FileName);
      cPath:= GetPath();
      OpenFile.InitialDir:= cPath;
      if cPath <> '' then
        OpenFile.InitialDir:=  OpenFile.InitialDir + '\text';
      OpenFile.Filter:='Ghost Master Text File|*.ut8';
      if OpenFile.Execute then
      begin
        OpenXML.InitialDir :=cPath;
        OpenXML.Filter := 'XML File|*.xml';
        if OpenXML.Execute then
        begin
          if cSavFile.ReadUTF8file(OpenFile.FileName, OpenXML.FileName) then
            cSavFile.AddFields(nil)
          else
          begin
            MessageDlg('Sav pattern not found!', mtCustom, [mbOK], 0);
          end;
        end;
      end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Caption:= 'Save File Editor ' + VERSION;
   Left:=(Screen.Width-Width)  div 2;
   Top:=(Screen.Height-Height) div 2;
end;

procedure TForm1.FileTreeBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
  var
    r:TRect;
begin
 inherited;
  R := Sender.GetDisplayRect(Node, 0, True);
  if Odd((R.Top-Sender.OffsetY) div Node.NodeHeight) then
  begin
    ItemColor := 16446961;
    EraseAction := eaColor;
  end;
end;

procedure TForm1.FileTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PPropertyData;
begin
  Data := Sender.GetNodeData(Node);
  if TextType = ttNormal then
    case Column of
    0:
      CellText := Data.DName;
    1:
      CellText := Data.DValue;
    2:
      CellText := Data.DType;
    end;
end;

procedure TForm1.itemSaveClick(Sender: TObject);
var
  Data: PPropertyData;
begin
  if cSavfile <> nil then
    Data:=fileTree.GetNodeData(fileTree.GetFirstLevel(0));
    savefile.FileName:= Data^.DName;
    if savefile.Execute then
      cSavfile.WriteFile(savefile.FileName);
end;

function TForm1.GetDirectoryPath(Folder:Integer): string;
var
  PIDL: PItemIDList;
  Path: LPSTR;
  AMalloc: IMalloc;
begin
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
  if SHGetPathFromIDList(PIDL, Path) then
    Result := Path;
  SHGetMalloc(AMalloc);
  AMalloc.Free(PIDL);
  StrDispose(Path);
end;

procedure TForm1.FileTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  if Column = 1 then
    EditLink := TPropertyEditLink.Create;
end;

procedure TForm1.FileTreeScroll(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
begin
  if Sender.IsEditing then Sender.updateEditBounds;
end;

procedure TForm1.FileTreeNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Data: PPropertyData;
begin
 if HitInfo.HitNode <> nil then
   begin
   if HitInfo.HitColumn = 1 then begin
      Data := Sender.GetNodeData(HitInfo.HitNode);
      case  Data.DBType of
        SavStringRes,SavInt,SavFloat,SavDword,SavBoolean,SavStringEnum,SavFetter,SavGhostAnim,SavSaveState,SavSaveType,SavAttribute,SavGhostType,SavTime,SavMultiplier,SavFlags,SavTrainingEnum,SavController,SavTimeStage,SavUInt,SavPower,SavPowerShop:
        begin
          Sender.EditNode(HitInfo.HitNode, 1);
        end;
      end;
    end;
  end;
end;
procedure TForm1.FileTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
  var Data: PPropertyData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TForm1.FileTreeEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
    var Data: PPropertyData;
begin
    Data := Sender.GetNodeData(Node);
    if Data.Changed then begin
      Data.Changed:= false;
      FileTree.Refresh;
    end;
end;

procedure TForm1.FileTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: PVirtualNode;
  Data, ParentData: PPropertyData;
  canAdd, canDelete: Boolean;
begin
  canAdd := false;
  canDelete := false;
  if FileTree.GetFirstSelected <> nil then
  begin
    Node := FileTree.GetFirstSelected;
    Data := FileTree.GetNodeData(Node);
    case Data.DBType of
      SavHaunters:
      begin
        canAdd:= true;
      end;
      SavHaunter:
      begin
        canDelete:= true;
      end;
    end;
  end;
  BtnAddHaunter1.Enabled:= canAdd;
  BtnDeleteHaunter1.Enabled:= canDelete;
end;

procedure TForm1.BtnAddHaunter1Click(Sender: TObject);
var
  listSaveFile: TSavFile;
  cHaunters: TStringList;
  i: integer;
begin
  //load new save file:
  OpenImport.InitialDir:= GetDirectoryPath(COMMON_DOCUMENTS) + SAVEDATA_PATH;
  OpenImport.Filter:= 'Ghost Master Save File|*.sav';
  if OpenImport.Execute then
  begin
    cHaunters:= TStringList.Create;
    for i:=0 to High(cSavFile.ghosts) do
    begin
      cHaunters.Add(cSavFile.ghosts[i].hData.model_file);
    end;
    AddHaunters.Form2.cbb1.Items.Clear;
    listSaveFile:= TSavFile.Create(nil);
    listSaveFile.filename:= extractfilename(OpenImport.FileName);
    listSaveFile.setMemoryStream(OpenImport.FileName);
    cSavFile.pSaveFile:= @listSaveFile;
    AddHaunters.Form2.cbb1.Items:= listSaveFile.CreateList(cSavFile);
    if AddHaunters.Form2.ShowModal = mrOk then
    begin

    end;
    cSavFile.pSaveFile:= nil;
    cHaunters.Free;
    listSaveFile.Free;
  end;
  //cSavFile.AddElement();
end;

procedure TForm1.BtnDeleteHaunter1Click(Sender: TObject);
begin
 cSavFile.DeleteElement();
end;

procedure TForm1.itemAboutClick(Sender: TObject);
begin
  MyMessageDlg('@Woitek1993' , mtInformation, [mbOK],['Ok'], 'Save File Editor ' + VERSION);
end;

function TForm1.MyMessageDlg(const Msg: string; DlgTypt: TmsgDlgType; button: TMsgDlgButtons; Caption: array of string; dlgcaption: string): Integer;
var
  aMsgdlg: TForm;
  i: Integer;
  Dlgbutton: Tbutton;
  Captionindex: Integer;
begin
  aMsgdlg := createMessageDialog(Msg, DlgTypt, button);
  aMsgdlg.Caption := dlgcaption;
  aMsgdlg.BiDiMode := bdRightToLeft;
  Captionindex := 0;
  for i := 0 to aMsgdlg.componentcount - 1 Do
  begin
    if (aMsgdlg.components[i] is Tbutton) then
    Begin
      Dlgbutton := Tbutton(aMsgdlg.components[i]);
      if Captionindex <= High(Caption) then
        Dlgbutton.Caption := Caption[Captionindex];
      inc(Captionindex);
    end;
  end;
  Result := aMsgdlg.Showmodal;
end;

function TForm1.GetPath(): string;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  r.RootKey := HKEY_CURRENT_USER;
  Result := '';
  if r.KeyExists('SOFTWARE\\WoitekTools\\') then
  begin
    r.OpenKey('SOFTWARE\\WoitekTools\\', false);
    if r.ValueExists('GhostData_Path') then
    begin
      Result := r.ReadString('GhostData_Path');
    end
    else
    begin
      r.WriteString('GhostData_Path', Result);
    end;
  end
  else
  begin
    r.CreateKey('SOFTWARE\\WoitekTools\\');
    r.OpenKey('SOFTWARE\\WoitekTools\\', false);
    r.WriteString('GhostData_Path', Result);
  end;
  r.Free;
end;

procedure TForm1.SetPath(path: string);
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  r.RootKey := HKEY_CURRENT_USER;
  r.CreateKey('SOFTWARE\\WoitekTools\\');
  r.OpenKey('SOFTWARE\\WoitekTools\\', false);
  r.WriteString('GhostData_Path', path);
  r.Free;
end;

end.
