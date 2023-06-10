unit SavClass;

interface

uses Windows, Classes, SysUtils, Dialogs, Forms, strUtils, VirtualTrees,
HaunterData, Controls, Graphics, NativeXML;

type
  TData = record
    pUtf8Strings: Pointer;
    pGhostAnimStrings: Pointer;
    pSaveStateStrings: Pointer;
    pGhostTypeStrings: Pointer;
    pScenarioNames: Pointer;
    pGhostClassName: Pointer;
    pFetterImgList: Pointer;
    pPowers: Pointer;
    pPowersFamily: Pointer;
  end;

type
  TTime = record
    days: Cardinal;
    hours: Cardinal;
    minutes: Cardinal;
    seconds: Cardinal;
  end;

type
  TMission = record
    score: Cardinal;
    time: TTime;
    multiplier: Cardinal;
    completed: Cardinal;
    unlocked: Cardinal;
    num_of_ghosts: Cardinal;
    mghosts: array of cardinal;
  end;

type
  SavDataTypes = (
    SavInt, SavDword, SavStringRes, SavBoolean, SavFloat, SavStructure, SavFile,
      SavStringEnum, SavFetter, SavTrainingEnum, SavMultiplier, SavGhostAnim,
      SavSaveState, SavSaveType, SavGhostType, SavAttribute, SavTime, SavFlags,
      SavController, SavTimeStage, SavUInt, SavPower, SavPowerShop, SavHaunter,
      SavHaunters
    );
type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    DName: string;
    DValue: string;
    PValue: Pointer;
    DType: string;
    DBType: SavDataTypes;
    Custom: Pointer;
    Changed: Boolean;
  end;

type
  TFile = class(TObject)
    constructor Create(tree: TCustomVirtualStringTree);
    destructor Destroy; override;
    function GetPointer: Pointer;
    procedure setMemoryStream(dir: string);
    procedure AddFields(CustomTree: TCustomVirtualStringTree); virtual;
    function AddTreeData(ANode: PVirtualNode; Name: string;
      p: Pointer; Btype: SavDataTypes; VReplace: string; custom: Pointer):
        PVirtualNode;
    procedure ReplaceText(Data: PPropertyData; VReplace: string);
    procedure WriteFile(filename: string); virtual;
    function ReadDword(): Cardinal;
    function ReadFloat(): Single;
    function ReadUnicodeString(length: integer): string;
    function IsBitSet(const value, bit: Integer): Boolean;
    function getStringList: TStringList;

  public
    filename: string;
    filePointer: Pointer;
    fileData: TMemoryStream;
    fileTree: TCustomVirtualStringTree;
    utf8Strings, GhostAnimStrings, SaveStateStrings, GhostTypeStrings,
      ScenarioNames, PowerFamily: TStringList;
    GhostClassName: array of TGClassWeakness;
    Powers: array of TPowerData;
    fetterImgList: TImageList;
    dataPointers: TData;
  end;

type
  TSavFile = class(TFile)
    constructor Create(tree: TCustomVirtualStringTree);
    destructor Destroy; override;
    procedure AddFields(CustomTree: TCustomVirtualStringTree); override;
    procedure WriteFile(filename: string); override;
    procedure ReadMission(var mission: TMission);
    procedure ReadHaunterData(var hd: THaunterData);
    function ReadUTF8file(utfpath,xmlpath: string): Boolean;
  public
    hash: Cardinal;
    version: Cardinal;
    save_state: Cardinal;
    save_type: Cardinal;
    gold_plasm: single;
    missions: array of TMission;
    flags: Cardinal;
    highscares: Cardinal;
    time_gate: Cardinal;
    num_ghosts: Cardinal;
    ghosts: array of THaunterData;
    windwalker: Cardinal;
  end;

const
  SavDataNames: array[SavDataTypes] of string = ('Int', 'Dword', 'String',
    'Boolean', 'Float', 'Structure', 'File', 'StringEnum', 'Fetter', 'TraningEnum',
    'Multiplier', 'GhostAnim', 'SaveState', 'SaveType', 'GhostType', 'Attribute',
    'Time', 'SavFlags', 'Controller', 'TimeStage', 'UInt', 'Power', 'ShopPower',
    'Haunter', 'Haunters');
  TextDataPath = '\Data\TextData\';
  OptBool: array[0..1] of string = ('False', 'True');
  SaveTypes: array[0..2] of string = ('Normal', 'Revisit', 'Completed');
  FetterNames: array[0..15] of integer = (1379, 1381, 1382, 1383, 1384, 1385,
    1386, 1387, 1388, 1389, 1390, 1391, 1392, 1393, 1394, 429);
  FlagNames: array[0..2] of string = ('Ghoulroom', 'Tutorial-No Plasm',
    'Tutorial-Ghoulroom');
  Multipliers: array[0..3] of string = ('1', '2', '3', '5');
  Sprites: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1616, 1622,
    1628, 1634);
  Disturbances: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1617,
    1623, 1629, 1635);
  Elementals: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1618,
    1624, 1630, 1636);
  Vapours: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1619, 1625,
    1631, 1637);
  Frighteners: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1620,
    1626, 1632, 1638);
  Horrors: array[0..8] of Integer = (1611, 1612, 1613, 1614, 1615, 1621, 1627,
    1633, 1639);

implementation
{$R icons.RES}

constructor TFile.Create(tree: TCustomVirtualStringTree);
begin
  fileData := TMemoryStream.Create;
  fileTree := tree;
end;

destructor TFile.Destroy;
begin
  fileData.Free;
  utf8Strings.Free;
  fetterImgList.Free;
  GhostAnimStrings.Free;
  SaveStateStrings.Free;
  GhostTypeStrings.Free;
  ScenarioNames.Free;
  PowerFamily.Free;
  GhostClassName := nil;
  inherited;
end;

procedure TFile.WriteFile(filename: string);
begin

end;

function TFile.GetPointer: Pointer;
begin
  Result := Pointer(Longword(fileData.Memory) + fileData.Position);
end;

procedure TFile.setMemoryStream(dir: string);
begin
  fileData.LoadFromFile(dir);
  filePointer := GetPointer;
end;

procedure TFile.AddFields(CustomTree: TCustomVirtualStringTree);
begin
  fileTree.Clear;
end;

function TFile.AddTreeData(ANode: PVirtualNode; Name: string;
  p: Pointer; Btype: SavDataTypes; VReplace: string; custom: Pointer):
    PVirtualNode;
var
  Data: PPropertyData;
begin
  Result := fileTree.AddChild(ANode);
  Data := fileTree.GetNodeData(Result);
  fileTree.ValidateNode(Result, False);
  Data^.DName := Name;
  Data^.PValue := p;
  Data^.DType := SavDataNames[Btype];
  Data^.DBType := Btype;
  Data^.Custom := custom;
  ReplaceText(Data, VReplace);
end;

procedure TFile.ReplaceText(Data: PPropertyData; VReplace: string);
var
  s: string;
  i: integer;
  p: Pointer;
begin
  s := '';
  case Data.DBtype of
    SavInt, SavAttribute, SavMultiplier, SavController, SavTimeStage, SavUInt: s
      := Format('%d', [Integer(Data.PValue^)]);
    SavDword: s := Format('%.*x', [8, Dword(Data.PValue^)]);
    SavBoolean: s := OptBool[Dword(Data.PValue^)];
    SavFloat: s := Format('%f', [Single(Data.PValue^)]);
    SavPower:
      begin
        s := utf8Strings[Powers[Dword(Data.PValue^)].name] + ' (' +
          PowerFamily[Powers[Dword(Data.PValue^)].family] + ')';
      end;
    SavPowerShop:
      begin
        p := Data.PValue;
        Inc(PCardinal(p), 1);
        s := utf8Strings[Powers[Dword(Data.PValue^)].name] + ' (' +
          PowerFamily[Powers[Dword(Data.PValue^)].family] + '),' +
          utf8Strings[Powers[Dword(p^)].name] + ' (' +
          PowerFamily[Powers[Dword(Data.PValue^)].family] + ')';
      end;
    SavTrainingEnum:
      begin
        case GhostClassName[THaunterData(Data.PValue^).gtype].GClass of
          1: s :=
            utf8strings[Sprites[THaunterData(Data.PValue^).parameters.training_level]];
          2: s :=
            utf8strings[Disturbances[THaunterData(Data.PValue^).parameters.training_level]];
          3: s :=
            utf8strings[Elementals[THaunterData(Data.PValue^).parameters.training_level]];
          4: s :=
            utf8strings[Vapours[THaunterData(Data.PValue^).parameters.training_level]];
          5: s :=
            utf8strings[Frighteners[THaunterData(Data.PValue^).parameters.training_level]];
          6: s :=
            utf8strings[Horrors[THaunterData(Data.PValue^).parameters.training_level]];
        end;
      end;
    SavTime:
      s := Format('%0.2d:%0.2d:%0.2d:%0.2d', [TTIme(Data.PValue^).days,
        TTIme(Data.PValue^).hours, TTIme(Data.PValue^).minutes,
        TTIme(Data.PValue^).seconds]);
    SavGhostType:
      s := GhostTypeStrings[THaunterData(Data.PValue^).gtype];
    SavSaveType:
      s := SaveTypes[Dword(Data.PValue^)];
    SavSaveState:
      s := SaveStateStrings[Dword(Data.PValue^)];
    SavGhostAnim:
      s := GhostAnimStrings[Dword(Data.PValue^)];
    SavStringEnum:
      s := utf8Strings[Integer(Data.PValue^)];
    SavFlags:
      begin
        for i := 0 to Length(FlagNames) - 1 do
        begin
          if IsBitSet(Integer(Data.PValue^), i) then
            if s = '' then
              s := FlagNames[i]
            else
              s := s + ',' + FlagNames[i];
        end;
      end;
    SavFetter:
      begin
        for i := 0 to Length(FetterNames) - 1 do
        begin
          if i = 15 then
          begin
            if IsBitSet(Integer(Data.PValue^), 17) then
            begin
              if s = '' then
                s := utf8Strings[FetterNames[i]]
              else
                s := s + ',' + utf8Strings[FetterNames[i]];
            end;
          end
          else if IsBitSet(Integer(Data.PValue^), i) then
            if s = '' then
              s := utf8Strings[FetterNames[i]]
            else
              s := s + ',' + utf8Strings[FetterNames[i]];
        end;
      end;
  end;
  //SavMultiplier:
  if VReplace <> '' then
    s := VReplace;
  Data.DValue := s;
end;

function TFile.ReadDword(): Cardinal;
begin
  Result := DWord(filePointer^);
  Inc(Longword(filePointer), 4);
end;

function TFile.ReadFloat(): Single;
begin
  Result := Single(filePointer^);
  Inc(Longword(filePointer), 4);
end;

function TFile.ReadUnicodeString(length: integer): string;
var
  i: integer;
  s: string;
begin
  SetLength(Result, length);
  s := '';
  for i := 0 to length - 1 do
  begin
    if Char(filePointer^) = '' then
    begin
      Inc(Longword(filePointer), (length - i) * 2);
      SetLength(Result, i);
      Result := s;
      break;
    end;
    s := s + Char(filePointer^);
    Inc(Longword(filePointer), 2);
  end;
end;

function TSavFile.ReadUTF8file(utfpath,xmlpath: string): Boolean;
var
  utf8File: TextFile;
  tempString, textPath, sgp: string;
  tempArray, delim, customSave: TStringList;
  index, i: integer;
  xmlData: TNativeXml;
  rootNode, strDataNode: TXmlNode;
begin
  Result := false;
  hash := ReadDword;
  version := ReadDword;
  if (version >= 6) or (version <= 7) then
  begin
    xmlData := TNativeXml.Create(nil);
    xmlData.LoadFromFile(xmlpath);

    save_state := ReadDword;
    save_type := ReadDword;
    gold_plasm := ReadFloat;
    if hash = 895783796 then
    begin
      if version = 6 then
        SetLength(missions, 14)
      else
        SetLength(missions, 15);
    end
    else
    begin
      SetLength(missions, 15);
    end;
    AssignFile(utf8File, filename);
    Reset(utf8File);
    textPath := ExtractFilePath(Application.ExeName) + TextDataPath;
    utf8Strings := TStringList.Create;
    utf8Strings.LoadFromFile(textPath + 'Strings.txt');
    tempString := '';
    while not Eof(utf8File) do
    begin
      ReadLn(utf8File, tempString);
      if (Length(tempString) > 0) then
      begin
        if (LeftStr(tempString, 2) <> '//') then
        begin
          tempArray := TStringList.Create;
          tempArray.Delimiter := ' ';
          tempArray.DelimitedText := tempString;
          index := utf8Strings.IndexOf(tempArray[0]);
          if (index <> -1) then
          begin
            utf8Strings[index] := tempArray[1];
          end;
          tempArray.Free;
        end;
      end;
    end;
    CloseFile(utf8File);
    GhostAnimStrings := TStringList.Create;
    GhostAnimStrings.LoadFromFile(textPath + 'GhostAnimations.txt');
    SaveStateStrings := TStringList.Create;
    SaveStateStrings.LoadFromFile(textPath + 'SaveStates.txt');
    for index := 0 to SaveStateStrings.Count - 1 do
    begin
      SaveStateStrings[index] := utf8Strings[StrToInt(SaveStateStrings[index])];
    end;
    GhostTypeStrings := TStringList.Create;
    GhostTypeStrings.LoadFromFile(textPath + 'GhostTypes.txt');
    for index := 0 to GhostTypeStrings.Count - 1 do
    begin
      GhostTypeStrings[index] := utf8Strings[StrToInt(GhostTypeStrings[index])];
    end;
    ScenarioNames := TStringList.Create;
    ScenarioNames.LoadFromFile(textPath + 'ScenarioNames.txt');
    tempArray := TStringList.Create;
    tempArray.LoadFromFile(textPath + 'GhostClassWeakness.txt');
    SetLength(GhostClassName, tempArray.Count);
    for index := 0 to tempArray.Count - 1 do
    begin
      delim := TStringList.Create;
      delim.Delimiter := ',';
      delim.DelimitedText := tempArray[index];
      GhostClassName[index].GClass := StrToInt(delim[0]);
      GhostClassName[index].GWeakness := StrToInt(delim[1]);
      delim.Free;
    end;
    tempArray.Free;

    PowerFamily := TStringList.Create;
    PowerFamily.LoadFromFile(textPath + 'PowerFamilies.txt');
    for index := 0 to PowerFamily.Count - 1 do
    begin
      PowerFamily[index] := utf8Strings[StrToInt(PowerFamily[index])];
    end;

    //Powers
    tempArray := TStringList.Create;
    tempArray.LoadFromFile(textPath + 'PowerData.txt');
    SetLength(Powers, tempArray.Count);
    for index := 0 to tempArray.Count - 1 do
    begin
      delim := TStringList.Create;
      delim.Delimiter := ',';
      delim.DelimitedText := tempArray[index];
      Powers[index].family := StrToInt(delim[0]);
      Powers[index].p_type := StrToInt(delim[1]);
      Powers[index].band := StrToInt(delim[2]);
      Powers[index].recharge_time := High(Cardinal) - StrToInt(delim[3]);
      Powers[index].unk := StrToInt(delim[4]);
      Powers[index].unk2 := StrToInt(delim[5]);
      Powers[index].name := StrToInt(delim[6]);
      Powers[index].description := StrToInt(delim[7]);
      delim.Free;
    end;
    tempArray.Free;
    Result := true;
    //break;
  end;
end;
// end;
// end;
//end;

function TFile.IsBitSet(const value, bit: Integer): Boolean;
begin
  Result := value and (1 shl bit) <> 0;
end;

function TFile.getStringList: TStringList;
begin
  Result := utf8Strings;
end;

constructor TSavFile.Create(tree: TCustomVirtualStringTree);
begin
  inherited Create(tree);
  fetterImgList := TImageList.Create(nil);
end;

destructor TSavFile.Destroy;
begin
  inherited;
end;

procedure TSavFile.ReadMission(var mission: TMission);
var
  i, powerIndex: integer;
begin
  mission.score := ReadDword;
  mission.time.days := ReadDword;
  mission.time.hours := ReadDword;
  mission.time.minutes := ReadDword;
  mission.time.seconds := ReadDword;
  mission.multiplier := ReadDword;
  mission.completed := ReadDword;
  mission.unlocked := ReadDword;
  mission.num_of_ghosts := ReadDword;
  SetLength(mission.mghosts, mission.num_of_ghosts);
  for i := 0 to mission.num_of_ghosts - 1 do
  begin
    mission.mghosts[i] := ReadDword;
  end;
end;

procedure TSavFile.ReadHaunterData(var hd: THaunterData);
var
  i, powerIndex: integer;
  p: Pointer;
begin
  hd.script := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.model_file := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.type_script := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.ghost_script := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.spawn := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.link_to_object := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.camera := PAnsiChar(filePointer);
  Inc(Longword(filePointer), 50);
  hd.anim_id := ReadDword;
  hd.gtype := ReadDword;
  hd.name := ReadDword;
  hd.bio := ReadDword;
  hd.haunter_state := ReadDword;
  //parameters
  hd.parameters.attention := ReadDword;
  hd.parameters.discipline := ReadDword;
  hd.parameters.intelligence := ReadDword;
  hd.parameters.training_level := ReadDword;
  hd.parameters.unlocked := ReadDword;
  hd.parameters.controller_type := ReadDword;

  powerIndex := Length(Powers) - 1;

  hd.parameters.power_1 := powerIndex;
  hd.parameters.power_2 := powerIndex;
  hd.parameters.power_3 := powerIndex;
  hd.parameters.power_4 := powerIndex;
  hd.parameters.power_5 := powerIndex;
  hd.parameters.power_6 := powerIndex;
  hd.parameters.power_7 := powerIndex;
  hd.parameters.power_8 := powerIndex;
  hd.parameters.power_9 := powerIndex;
  hd.parameters.power_10 := powerIndex;

  for i := 0 to 9 do
  begin
    powerIndex := ReadDword;
    case Powers[powerIndex].band of
      1: hd.parameters.power_1 := powerIndex;
      2: hd.parameters.power_2 := powerIndex;
      3: hd.parameters.power_3 := powerIndex;
      4: hd.parameters.power_4 := powerIndex;
      5: hd.parameters.power_5 := powerIndex;
      6: hd.parameters.power_6 := powerIndex;
      7: hd.parameters.power_7 := powerIndex;
      8: hd.parameters.power_8 := powerIndex;
      9: hd.parameters.power_9 := powerIndex;
      10: hd.parameters.power_10 := powerIndex;
    end;
  end;
  hd.parameters.spower_1_1 := ReadDword;
  hd.parameters.spower_1_2 := ReadDword;
  hd.parameters.spower_2_1 := ReadDword;
  hd.parameters.spower_2_2 := ReadDword;
  hd.parameters.spower_3_1 := ReadDword;
  hd.parameters.spower_3_2 := ReadDword;
  hd.parameters.spower_4_1 := ReadDword;
  hd.parameters.spower_4_2 := ReadDword;
  hd.parameters.spower_5_1 := ReadDword;
  hd.parameters.spower_5_2 := ReadDword;
  hd.parameters.spower_6_1 := ReadDword;
  hd.parameters.spower_6_2 := ReadDword;
  hd.parameters.spower_7_1 := ReadDword;
  hd.parameters.spower_7_2 := ReadDword;
  hd.parameters.spower_8_1 := ReadDword;
  hd.parameters.spower_8_2 := ReadDword;
  hd.parameters.spower_9_1 := ReadDword;
  hd.parameters.spower_9_2 := ReadDword;
  hd.parameters.spower_10_1 := ReadDword;
  hd.parameters.spower_10_2 := ReadDword;
  //rest
  hd.fetter := ReadDword;
  hd.experience := ReadDword;
  hd.time_gate := ReadDword;
end;

procedure TSavFile.AddFields(CustomTree: TCustomVirtualStringTree);
var
  root, missionsNode, tempNode, missionNode, ghostsNode, haunterNode,
    paramsNode, powerNode: PVirtualNode;
  i, j: integer;
  bitmapFile: TBitmap;
  customSave, delim: TStringList;
  RS: TResourceStream;
begin
  //load fetter icons
  RS := TResourceStream.Create(HInstance, 'electrical', RT_RCDATA);
  bitmapFile := TBitmap.Create;
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'mirror', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'air', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'fire', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'water', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'corpse', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'child', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'inside', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'earth', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'outside', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'thoroughfare', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'emotional', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'violence', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'murder', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'sleep', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  RS := TResourceStream.Create(HInstance, 'headless', RT_RCDATA);
  bitmapFile.LoadFromStream(RS);
  fetterImgList.AddMasked(bitmapFile, clfuchsia);
  RS.Free;
  bitmapFile.Free;

  dataPointers.pUtf8Strings := @utf8Strings;
  dataPointers.pGhostAnimStrings := @GhostAnimStrings;
  dataPointers.pSaveStateStrings := @SaveStateStrings;
  dataPointers.pGhostTypeStrings := @GhostTypeStrings;
  dataPointers.pScenarioNames := @ScenarioNames;
  dataPointers.pGhostClassName := @GhostClassName;
  dataPointers.pFetterImgList := @fetterImgList;
  dataPointers.pPowers := @Powers;
  dataPointers.pPowersFamily := @PowerFamily;

  //load all variables

  for i := 0 to Length(missions) - 1 do
  begin
    ReadMission(missions[i]);
  end;
  flags := ReadDword;
  highscares := ReadDword;
  time_gate := ReadDword;
  num_ghosts := ReadDword;
  SetLength(ghosts, num_ghosts);
  for i := 0 to num_ghosts - 1 do
  begin
    ReadHaunterData(ghosts[i]);
  end;
  windwalker := ReadDword;

  //Add Tree
  AddTreeData(nil, ExtractFileName(filename), nil, SavFile, '', nil);
  root := fileTree.GetFirstLevel(0);
  //main
  AddTreeData(root, 'Hash', @hash, SavDword, '', nil);
  AddTreeData(root, 'Version', @version, SavUInt, '', nil);
  AddTreeData(root, 'Save State', @save_state, SavSaveState, '',
    @SaveStateStrings);
  AddTreeData(root, 'Save Type', @save_type, SavSaveType, '', nil);
  AddTreeData(root, 'Gold Plasm', @gold_plasm, SavFloat, '', nil);
  missionsNode := AddTreeData(root, 'Missions', nil, SavStructure, '', nil);
  for i := 0 to Length(missions) - 1 do
  begin
    missionNode := AddTreeData(missionsNode,
      utf8Strings[StrToInt(ScenarioNames[i])], nil, SavStructure, '', nil);
    AddTreeData(missionNode, 'Score', @missions[i].score, SavUInt, '', nil);
    AddTreeData(missionNode, 'Time', @missions[i].time, SavTime, '', nil);
    AddTreeData(missionNode, 'Multiplier', @missions[i].multiplier,
      SavMultiplier, '', nil);
    AddTreeData(missionNode, 'Completed', @missions[i].completed, SavBoolean,
      '', nil);
    AddTreeData(missionNode, 'Unlocked', @missions[i].unlocked, SavBoolean, '',
      nil);
    tempNode := AddTreeData(missionNode, 'Lay To Rest Ghosts', nil, SavStructure,
      '', nil);
    for j := 0 to missions[i].num_of_ghosts - 1 do
      AddTreeData(tempNode, 'Ghost ' + IntToStr(j + 1), @missions[i].mghosts[j],
        SavStringEnum, '', @utf8Strings);
  end;
  AddTreeData(root, 'Flags', @flags, SavFlags, '', nil);
  AddTreeData(root, 'Highscares', @highscares, SavBoolean, '', nil);
  AddTreeData(root, 'Time Gate', @time_gate, SavUInt, '', nil);
  ghostsNode := AddTreeData(root, 'Ghosts', nil, SavHaunters, '', @ghosts);
  for i := 0 to Length(ghosts) - 1 do
  begin
    haunterNode := AddTreeData(ghostsNode, utf8Strings[ghosts[i].name], nil,
      SavHaunter, '', @ghosts[i]);
    AddTreeData(haunterNode, 'Script', @ghosts[i].script, SavStringRes,
      ghosts[i].script, nil);
    AddTreeData(haunterNode, 'File', @ghosts[i].model_file, SavStringRes,
      ghosts[i].model_file, nil);
    AddTreeData(haunterNode, 'Type', @ghosts[i].type_script, SavStringRes,
      ghosts[i].type_script, nil);
    AddTreeData(haunterNode, 'Ghost Script', @ghosts[i].ghost_script,
      SavStringRes, ghosts[i].ghost_script, nil);
    AddTreeData(haunterNode, 'Spawn', @ghosts[i].spawn, SavStringRes,
      ghosts[i].spawn, nil);
    AddTreeData(haunterNode, 'Link', @ghosts[i].link_to_object, SavStringRes,
      ghosts[i].link_to_object, nil);
    AddTreeData(haunterNode, 'Camera', @ghosts[i].camera, SavStringRes,
      ghosts[i].camera, nil);
    AddTreeData(haunterNode, 'Animation ID', @ghosts[i].anim_id, SavGhostAnim,
      '', @GhostAnimStrings);
    AddTreeData(haunterNode, 'Type', @ghosts[i], SavGhostType, '',
      @dataPointers);
    AddTreeData(haunterNode, 'Name', @ghosts[i].name, SavStringEnum, '',
      @utf8Strings);
    AddTreeData(haunterNode, 'Bio', @ghosts[i].bio, SavStringEnum, '',
      @utf8Strings);
    AddTreeData(haunterNode, 'Haunter State', @ghosts[i].haunter_state, SavUInt,
      '', nil);
    paramsNode := AddTreeData(haunterNode, 'Parameters', nil, SavStructure, '',
      nil);
    //params
    AddTreeData(paramsNode, 'Attention', @ghosts[i].parameters.attention,
      SavAttribute, '', nil);
    AddTreeData(paramsNode, 'Discipline', @ghosts[i].parameters.discipline,
      SavAttribute, '', nil);
    AddTreeData(paramsNode, 'Intelligence', @ghosts[i].parameters.intelligence,
      SavAttribute, '', nil);
    AddTreeData(paramsNode, 'Training Level', @ghosts[i], SavTrainingEnum, '',
      @dataPointers);
    AddTreeData(paramsNode, 'Unlocked', @ghosts[i].parameters.unlocked,
      SavBoolean, '', nil);
    AddTreeData(paramsNode, 'Controller', @ghosts[i].parameters.controller_type,
      SavController, '', nil);
    //Powers
    powerNode := AddTreeData(paramsNode, 'Powers', nil, SavStructure, '', nil);
      //Unsorted!
    AddTreeData(powerNode, 'Power 1', @ghosts[i].parameters.power_1, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 2', @ghosts[i].parameters.power_2, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 3', @ghosts[i].parameters.power_3, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 4', @ghosts[i].parameters.power_4, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 5', @ghosts[i].parameters.power_5, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 6', @ghosts[i].parameters.power_6, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 7', @ghosts[i].parameters.power_7, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 8', @ghosts[i].parameters.power_8, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 9', @ghosts[i].parameters.power_9, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Power 10', @ghosts[i].parameters.power_10, SavPower,
      '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 1', @ghosts[i].parameters.spower_1_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 2', @ghosts[i].parameters.spower_2_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 3', @ghosts[i].parameters.spower_3_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 4', @ghosts[i].parameters.spower_4_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 5', @ghosts[i].parameters.spower_5_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 6', @ghosts[i].parameters.spower_6_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 7', @ghosts[i].parameters.spower_7_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 8', @ghosts[i].parameters.spower_8_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 9', @ghosts[i].parameters.spower_9_1,
      SavPowerShop, '', @dataPointers);
    AddTreeData(powerNode, 'Shop Power 10', @ghosts[i].parameters.spower_10_1,
      SavPowerShop, '', @dataPointers);
    //Rest
    AddTreeData(haunterNode, 'Fetter', @ghosts[i].fetter, SavFetter, '',
      @dataPointers);
    AddTreeData(haunterNode, 'Experience', @ghosts[i].experience, SavUInt, '',
      nil);
    AddTreeData(haunterNode, 'Time Gate Stage', @ghosts[i].time_gate,
      SavTimeStage, '', nil);
  end;
  AddTreeData(root, 'Windwalker', @windwalker, SavBoolean, '', nil);
end;

procedure TSavFile.WriteFile(filename: string);
var
  fs: TFileStream;
  i, j, k, temp: integer;
  p: Pointer;
begin
  fs := TFileStream.Create(filename, fmCreate);
  fs.Write(hash, SizeOf(hash));
  fs.Write(version, SizeOf(version));
  fs.Write(save_state, SizeOf(save_state));
  fs.Write(save_type, SizeOf(save_type));
  fs.Write(gold_plasm, SizeOf(gold_plasm));
  for i := 0 to Length(missions) - 1 do
  begin
    fs.Write(missions[i].score, SizeOf(missions[i].score));
    fs.Write(missions[i].time.days, SizeOf(missions[i].time.days));
    fs.Write(missions[i].time.hours, SizeOf(missions[i].time.hours));
    fs.Write(missions[i].time.minutes, SizeOf(missions[i].time.minutes));
    fs.Write(missions[i].time.seconds, SizeOf(missions[i].time.seconds));
    fs.Write(missions[i].multiplier, SizeOf(missions[i].multiplier));
    fs.Write(missions[i].completed, SizeOf(missions[i].completed));
    fs.Write(missions[i].unlocked, SizeOf(missions[i].unlocked));
    fs.Write(missions[i].num_of_ghosts, SizeOf(missions[i].num_of_ghosts));
    for j := 0 to missions[i].num_of_ghosts - 1 do
    begin
      fs.Write(missions[i].mghosts[j], SizeOf(missions[i].mghosts[j]));
    end;
  end;
  fs.Write(flags, SizeOf(flags));
  fs.Write(highscares, SizeOf(highscares));
  fs.Write(time_gate, SizeOf(time_gate));
  fs.Write(num_ghosts, SizeOf(num_ghosts));
  for i := 0 to num_ghosts - 1 do
  begin
    fs.Write(ghosts[i].script[1], SizeOf(ghosts[i].script) - 1);
    fs.Write(ghosts[i].model_file[1], SizeOf(ghosts[i].model_file) - 1);
    fs.Write(ghosts[i].type_script[1], SizeOf(ghosts[i].type_script) - 1);
    fs.Write(ghosts[i].ghost_script[1], SizeOf(ghosts[i].ghost_script) - 1);
    fs.Write(ghosts[i].spawn[1], SizeOf(ghosts[i].spawn) - 1);
    fs.Write(ghosts[i].link_to_object[1], SizeOf(ghosts[i].link_to_object) - 1);
    fs.Write(ghosts[i].camera[1], SizeOf(ghosts[i].camera) - 1);
    fs.Write(ghosts[i].anim_id, SizeOf(ghosts[i].anim_id));
    fs.Write(ghosts[i].gtype, SizeOf(ghosts[i].gtype));
    fs.Write(ghosts[i].name, SizeOf(ghosts[i].name));
    fs.Write(ghosts[i].bio, SizeOf(ghosts[i].bio));
    fs.Write(ghosts[i].haunter_state, SizeOf(ghosts[i].haunter_state));
    //parameters
    fs.Write(ghosts[i].parameters.attention,
      SizeOf(ghosts[i].parameters.attention));
    fs.Write(ghosts[i].parameters.discipline,
      SizeOf(ghosts[i].parameters.discipline));
    fs.Write(ghosts[i].parameters.intelligence,
      SizeOf(ghosts[i].parameters.intelligence));
    fs.Write(ghosts[i].parameters.training_level,
      SizeOf(ghosts[i].parameters.training_level));
    fs.Write(ghosts[i].parameters.unlocked,
      SizeOf(ghosts[i].parameters.unlocked));
    fs.Write(ghosts[i].parameters.controller_type,
      SizeOf(ghosts[i].parameters.controller_type));
    //function for
    p := @ghosts[i].parameters.power_1;
    temp := Length(Powers) - 1;
    k := 0;
    for j := 0 to 9 do
    begin
      if Integer(p^) = temp then
        Inc(k)
      else
        fs.Write(Integer(p^), SizeOf(k));
      Inc(PCardinal(p), 1);
    end;
    for j := 0 to k - 1 do
    begin
      fs.Write(temp, SizeOf(temp));
    end;
    fs.Write(ghosts[i].parameters.spower_1_1,
      SizeOf(ghosts[i].parameters.spower_1_1));
    fs.Write(ghosts[i].parameters.spower_1_2,
      SizeOf(ghosts[i].parameters.spower_1_2));
    fs.Write(ghosts[i].parameters.spower_2_1,
      SizeOf(ghosts[i].parameters.spower_2_1));
    fs.Write(ghosts[i].parameters.spower_2_2,
      SizeOf(ghosts[i].parameters.spower_2_2));
    fs.Write(ghosts[i].parameters.spower_3_1,
      SizeOf(ghosts[i].parameters.spower_3_1));
    fs.Write(ghosts[i].parameters.spower_3_2,
      SizeOf(ghosts[i].parameters.spower_3_2));
    fs.Write(ghosts[i].parameters.spower_4_1,
      SizeOf(ghosts[i].parameters.spower_4_1));
    fs.Write(ghosts[i].parameters.spower_4_2,
      SizeOf(ghosts[i].parameters.spower_4_2));
    fs.Write(ghosts[i].parameters.spower_5_1,
      SizeOf(ghosts[i].parameters.spower_5_1));
    fs.Write(ghosts[i].parameters.spower_5_2,
      SizeOf(ghosts[i].parameters.spower_5_2));
    fs.Write(ghosts[i].parameters.spower_6_1,
      SizeOf(ghosts[i].parameters.spower_6_1));
    fs.Write(ghosts[i].parameters.spower_6_2,
      SizeOf(ghosts[i].parameters.spower_6_2));
    fs.Write(ghosts[i].parameters.spower_7_1,
      SizeOf(ghosts[i].parameters.spower_7_1));
    fs.Write(ghosts[i].parameters.spower_7_2,
      SizeOf(ghosts[i].parameters.spower_7_2));
    fs.Write(ghosts[i].parameters.spower_8_1,
      SizeOf(ghosts[i].parameters.spower_8_1));
    fs.Write(ghosts[i].parameters.spower_8_2,
      SizeOf(ghosts[i].parameters.spower_8_2));
    fs.Write(ghosts[i].parameters.spower_9_1,
      SizeOf(ghosts[i].parameters.spower_9_1));
    fs.Write(ghosts[i].parameters.spower_9_2,
      SizeOf(ghosts[i].parameters.spower_9_2));
    fs.Write(ghosts[i].parameters.spower_10_1,
      SizeOf(ghosts[i].parameters.spower_10_1));
    fs.Write(ghosts[i].parameters.spower_10_2,
      SizeOf(ghosts[i].parameters.spower_10_2));
    //rest
    fs.Write(ghosts[i].fetter, SizeOf(ghosts[i].fetter));
    fs.Write(ghosts[i].experience, SizeOf(ghosts[i].experience));
    fs.Write(ghosts[i].time_gate, SizeOf(ghosts[i].time_gate));
  end;
  fs.Write(windwalker, SizeOf(windwalker));
  fs.Free;
end;
end.

