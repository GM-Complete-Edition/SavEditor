unit Editors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons,
  Mask,SavClass,CheckLst,CheckCombo,FloatSpinEdit,IntSpinEdit,HaunterData,SearchComboBox;

type
  // Describes the type of value a property tree node stores in its data property.
  TValueType = (
    vtNone,
    vtString,
    vtPickString,
    vtNumber,
    vtPickNumber,
    vtMemo,
    vtDate
  );

//----------------------------------------------------------------------------------------------------------------------

  // Our own edit link to implement several different node editors.
  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit,FEdit2: TWinControl;
    FNumEdit: Integer;
    FCheckList: Boolean;
    FRect: TRect;
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HexEditKeyPress(Sender: TObject; var Key: Char);
    function IsBitSet(const value: Cardinal; const bit: Byte): Boolean;
    procedure SetBit(var value: Integer; const Bit: Byte);
    procedure HexEditChange(Sender: TObject);
    procedure TimeEditChange(Sender: TObject);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

//----------------------------------------------------------------------------------------------------------------------

type
  TPropertyTextKind = (
    ptkText,
    ptkHint
  );

//----------------------------------------------------------------------------------------------------------------------

implementation


//----------------- TPropertyEditLink ----------------------------------------------------------------------------------

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TPropertyEditLink.Destroy;

begin
  if FNumEdit = 1 then
    FEdit2.Free;
  if (Assigned(FEdit)) and (FEdit.HandleAllocated) then
    PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  case Key of
    VK_RETURN:
      begin
        FTree.EndEditNode;
        Key := 0;
      end;
  end;
end;

procedure TPropertyEditLink.HexEditKeyPress(Sender: TObject;
  var Key: Char);
begin
if not (Key in [#3,#27,#22,#127, '0'..'9', 'a'..'f', 'A'..'F']) then
      key := #0;  
end;

procedure TPropertyEditLink.KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
  #27,#13:  Key:=#0;
  end;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: Boolean;

begin
  Result := True;
  if FNumEdit = 1 then
    FEdit2.Show;
  FEdit.Show;
  FEdit.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: Boolean;

begin
  Result := True;
  FEdit.Hide;
end;

//----------------------------------------------------------------------------------------------------------------------
const
ConvertHex: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
function HexToByte (S:Pchar):Byte;   
begin
   result := Byte((ConvertHex[S[0]] shl 4) + ConvertHex[S[1]]);
end;

function TPropertyEditLink.IsBitSet(const value: Cardinal; const bit: Byte): Boolean;
begin
  Result := value and (1 shl bit) <> 0;
end;

procedure TPropertyEditLink.SetBit(var value: Integer; const Bit: Byte);
begin
  value:= value or (1 shl Bit);
end;

procedure TPropertyEditLink.HexEditChange(Sender: TObject);
var
 i: Integer;
 s: String;
 pz: Integer;
begin
s := TEdit(sender).Text;
 pz:= TMaskEdit(sender).SelStart;
 for i := Length(s) downto 1 do
 begin
   if i=9 then begin  s[i]:=' '; continue; end;
   if not (s[i] in ['0'..'9','A'..'F']) then s[i]:='0';
 end;
 TEdit(sender).Text := s;
 TMaskEdit(sender).SelStart := pz;
end;

procedure TPropertyEditLink.TimeEditChange(Sender: TObject);
var
 i: Integer;
 s: String;
 temp: TStringList;
begin
 s := TEdit(sender).Text;
 temp:= TStringList.Create;
 temp.Delimiter:= ':';
 temp.DelimitedText:= s;
 i:= StrToInt(temp[0]);
 temp[0]:= Format('%0.2d',[i]);
 i:= StrToInt(temp[1]);
 if i > 23 then
  temp[1]:= Format('%0.2d',[23])
 else
  temp[1]:= Format('%0.2d',[i]);
 i:= StrToInt(temp[2]);
 if i > 59 then
  temp[2]:= Format('%0.2d',[59])
 else
  temp[2]:= Format('%0.2d',[i]);
 i:= StrToInt(temp[3]);
 if i > 59 then
  temp[3]:= Format('%0.2d',[59])
 else
  temp[3]:= Format('%0.2d',[i]);
 TEdit(sender).Text := temp[0] + ':' + temp[1] + ':' + temp[2] + ':' + temp[3];
 temp.free;
end;
 type
  ClassWeakness = array of TGClassWeakness;
  PClassWeakness = ^ClassWeakness;
  ClassPower = array of TPowerData;
  PClassPower = ^ClassPower;
function TPropertyEditLink.EndEdit: Boolean;
var
  Data,tempData: PPropertyData;
  bytes: array[0..15]of byte;
  i:Integer;
  b:Integer;
  s,s2:string;
  pp:Pchar;
  fp,bp:Cardinal;
  fsInt: SmallInt; fByte: Byte;
  fInt,kCB1,kCB2,eInt: integer; fFloat:Single;
  fFlags: Cardinal;
  tempClassWeakness: ClassWeakness;
  tempNode: PVirtualNode;
  temp: TStringList;
  tempPowerData: ClassPower;
  p: Pointer;
  procedure SetPower(p: pointer; value: integer; Edit: TWinControl);
  var
    i: integer;
  begin
    if Data.DName <> TSearchComboBox(Edit).Text then
    begin
      tempPowerData:= PClassPower(TData(Data.Custom^).pPowers)^;
      temp:= TStringList(TData(Data.Custom^).pPowersFamily^);
      s:= TSearchComboBox(Edit).Text;
      s2:= copy(s,ansipos('(',s)+1,ansipos(')',s) -ansipos('(',s)-1);
      b:= temp.IndexOf(s2);
      temp:= TStringList(TData(Data.Custom^).pUtf8Strings^);
      SetLength(s,ansipos('(',s)-2) ;
      for i:=0 to length(tempPowerData) -1 do
      begin
        if tempPowerData[i].family = b then
          if temp[tempPowerData[i].name] = s then
            begin
              case value of
               0: Data.DValue:= TSearchComboBox(Edit).Text;
               1: Data.DValue:= TSearchComboBox(Edit).Text + ',';
               2: Data.DValue:= Data.DValue + TSearchComboBox(Edit).Text;
              end;
              Integer(p^) := i;
              Data.Changed:=True;
              FTree.InvalidateNode(FNode);
            end;
        end;
      end;
    end;

  function FindNode(node: PVirtualNode; text: string): PVirtualNode;
  var
    tempNode: PVirtualNode;
    data: PPropertyData;
  begin
    tempNode := FTree.GetFirstChild(node);
    Result:= nil;
    while Assigned(tempNode) do
    begin
      data:= FTree.GetNodeData(tempNode);
      if data.DName = text then
      begin
          Result:= tempNode;
          break;
      end;
      tempNode := FTree.GetNextSiblingNoInit(tempNode);
    end;
  end;

  procedure ComboBoxDefault;
  begin
    fInt:=TSearchComboBox(FEdit).ItemIndex;
    if fInt <> Integer(Data.PValue^) then
    begin
      Data.DValue := TSearchComboBox(FEdit).Text;
      Integer(Data.PValue^) :=fInt;
      Data.Changed:=True;
      FTree.InvalidateNode(FNode);
    end;
  end;

begin
  Result := True;

  Data := FTree.GetNodeData(FNode);

  case Data.DBType of
    SavStringRes:
      begin
        s:=TEdit(FEdit).Text;
        if s <> Data.DValue then
        begin
          String(Data.PValue^):= s;
          Data.DValue:= s;
          Data.Changed:=True;
          FTree.InvalidateNode(FNode);
        end;
      end;
     SavTime:
      begin
       s:=TEdit(FEdit).Text;
        if S <> Data.DValue then
        begin
          Data.DValue := s;
          temp:= TStringList.Create;
          temp.Delimiter:= ':';
          temp.DelimitedText:= s;
          if temp[0] = '' then
            TTime(Data.PValue^).days:= 0
          else
            TTime(Data.PValue^).days:= StrToInt(temp[0]);
          if temp[1] = '' then
          TTime(Data.PValue^).hours:= 0
          else
            TTime(Data.PValue^).hours:= StrToInt(temp[1]);
          if temp[2] = '' then
            TTime(Data.PValue^).minutes:= 0
          else
            TTime(Data.PValue^).minutes:= StrToInt(temp[2]);
          if temp.Count = 3 then
            TTime(Data.PValue^).seconds:= 0
          else
            TTime(Data.PValue^).seconds:= StrToInt(temp[3]);
          temp.Free;
          Data.Changed:=True;
          FTree.InvalidateNode(FNode);
        end;
      end;
    SavDword:
      begin
        s:=TEdit(FEdit).Text;
        if S <> Data.DValue then
        begin
          Data.DValue := S;
          b:=7;
          for i:=0 to 3 do begin
            bytes[i]:=HexToByte(@s[b]);
            Dec(b,2);
          end;
          Move(bytes[0],Data.PValue^,4);
          Data.Changed:=True;
          FTree.InvalidateNode(FNode);
        end;
      end;
    SavPower:
    begin
      SetPower(Data.PValue,0, FEdit);
    end;
    SavPowerShop:
    begin
      SetPower(Data.PValue, 1, FEdit);
      p:= Data.PValue;
      Inc(PCardinal(p),1);
      SetPower(p,2, FEdit2);
    end;
    SavInt,SavAttribute,SavTimeStage,SavUInt,SavController:
      begin
        fInt:=TTreeSpinEdit(FEdit).Value;
        if fInt <> Integer(Data.PValue^) then
        begin
          Data.DValue := IntToStr(fInt);
          Integer(Data.PValue^) :=fInt;
          Data.Changed:=True;
          FTree.InvalidateNode(FNode);
        end;
      end;
    SavFloat:
      begin
        fFloat:=TFloatSpinEdit(FEdit).Value;
        if fFloat <> Single(Data.PValue^) then
        begin
          Data.DValue := Format('%.2f',[fFloat]);
          Single(Data.PValue^) :=fFloat;
          Data.Changed:=True;
          FTree.InvalidateNode(FNode);
        end;
      end;
      SavGhostType:
      if TSearchComboBox(FEdit).Items.IndexOf(TSearchComboBox(FEdit).Text) <> -1 then
      begin
        fInt:=TSearchComboBox(FEdit).ItemIndex;
        tempNode:= FindNode(FNode.Parent,'Parameters');
        if tempNode <> nil then
        begin
          tempNode:= FindNode(tempNode,'Training Level');
          if tempNode <> nil then
          begin
            if fInt <> THaunterData(Data.PValue^).gtype then
            begin
              tempClassWeakness:= PClassWeakness(TData(Data.Custom^).pGhostClassName)^;
              if tempClassWeakness[fInt].GClass <> tempClassWeakness[THaunterData(Data.PValue^).gtype].GClass then
                begin
                  temp:= TStringList(TData(Data.Custom^).pUtf8Strings^);
                  i:= THaunterData(Data.PValue^).parameters.training_level;
                  TempData:= FTree.GetNodeData(tempNode);
                  case tempClassWeakness[fInt].GClass of
                    1: TempData.DValue:= temp[Sprites[i]];
                    2: TempData.DValue:= temp[Disturbances[i]];
                    3: TempData.DValue:= temp[Elementals[i]];
                    4: TempData.DValue:= temp[Vapours[i]];
                    5: TempData.DValue:= temp[Frighteners[i]];
                    6: TempData.DValue:= temp[Horrors[i]];
                  end;
                FTree.InvalidateNode(tempNode);
                end;
            end;
          end;
        end;
        Data.DValue := TSearchComboBox(FEdit).Text;
        THaunterData(Data.PValue^).gtype :=fInt;
        Data.Changed:=True;
        FTree.InvalidateNode(FNode);
        end;
      SavTrainingEnum:
      if TSearchComboBox(FEdit).Items.IndexOf(TSearchComboBox(FEdit).Text) <> -1 then
        begin
          fInt:=TSearchComboBox(FEdit).ItemIndex;
          if fInt <> THaunterData(Data.PValue^).parameters.training_level then
          begin
            Data.DValue := TSearchComboBox(FEdit).Text;
            THaunterData(Data.PValue^).parameters.training_level :=fInt;
            Data.Changed:=True;
            FTree.InvalidateNode(FNode);
          end;
        end;
      SavBoolean,SavStringEnum,SavGhostAnim,SavSaveState,SavSaveType:
      if TSearchComboBox(FEdit).Items.IndexOf(TSearchComboBox(FEdit).Text) <> -1 then
        begin
          fInt:=TSearchComboBox(FEdit).ItemIndex;
          if fInt <> Integer(Data.PValue^) then
          begin
            Data.DValue := TSearchComboBox(FEdit).Text;
            Integer(Data.PValue^) :=fInt;
            Data.Changed:=True;
            FTree.InvalidateNode(FNode);
          end;
        end;
      SavMultiplier:
        if TSearchComboBox(FEdit).Items.IndexOf(TSearchComboBox(FEdit).Text) <> -1 then
        begin
          fInt:=StrToInt(TSearchComboBox(FEdit).Text);
          if fInt <> Integer(Data.PValue^) then
          begin
            Data.DValue := TSearchComboBox(FEdit).Text;
            Integer(Data.PValue^) :=fInt;
            Data.Changed:=True;
            FTree.InvalidateNode(FNode);
          end;
        end;
      SavFlags:
        begin
          b:=0;
          for i:=0 to TCheckedComboBox(FEdit).Items.Count -1 do
          begin
            if TCheckedComboBox(FEdit).Checked[i] then
              SetBit(b,i);
          end;
            if b <> Integer(Data.PValue^) then
            begin
              Data.DValue:= TCheckedComboBox(FEdit).GetText;
              Integer(Data.PValue^) :=b;
              Data.Changed:=True;
              FTree.InvalidateNode(FNode);
            end;
        end;
      SavFetter:
      begin
        b:=0;
        for i:=0 to TCheckedComboBox(FEdit).Items.Count -1 do
        begin
          if TCheckedComboBox(FEdit).Checked[i] then
          begin
            if i = 15 then
              SetBit(b,17)
            else
              SetBit(b,i);
          end;
        end;
          if b <> Integer(Data.PValue^) then
          begin
            Data.DValue:= TCheckedComboBox(FEdit).GetText;
            Integer(Data.PValue^) :=b;
            Data.Changed:=True;
            FTree.InvalidateNode(FNode);
          end;
      end;
  end;
  FEdit.Hide;
  FTree.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;
begin
  Result := FRect;
end;

//----------------------------------------------------------------------------------------------------------------------
function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

var
  Data: PPropertyData;
  i,value: integer;
  strlist,temp: TStringList;
  tempClassWeakness: ClassWeakness;
  tempPowerData: ClassPower;
  p:Pointer;
  function MakePowerComboBox(index: integer):TSearchComboBox;
  var
    j: integer;
  begin
    Result := TSearchComboBox.Create(nil);
    with Result as TSearchComboBox do
    begin
      Visible := False;
      Parent := Tree;
      strlist:= TStringList.Create;
      strlist.Delimiter:= ' ';
      strlist.DelimitedText:= Data.DName;
      value:= StrToInt(strlist[strlist.Count-1]); //Power 1 we take 1(so we know the band)
      strlist.Free;
      tempPowerData:= PClassPower(TData(Data.Custom^).pPowers)^;
      strlist:= TStringList(TData(Data.Custom^).pUtf8Strings^);
      temp:= TStringList(TData(Data.Custom^).pPowersFamily^);
      Items.Add(strlist[tempPowerData[length(tempPowerData) - 1].name] + ' (' + temp[tempPowerData[length(tempPowerData) - 1].family] +')');
      for j := Low(tempPowerData) to High(tempPowerData) do
      begin
        if tempPowerData[j].band = value then
        begin
          Items.Add(strlist[tempPowerData[j].name] + ' (' + temp[tempPowerData[j].family] + ')');
        end;
      end;
      SendMessage(GetWindow(Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
      OnKeyUp := EditKeyUp;
      OnKeyDown:=EditKeyDown;
      ItemIndex:= Items.IndexOf(strlist[tempPowerData[index].name] + ' (' + temp[tempPowerData[index].family] + ')');
    end;
  end;
  function MakeCombobox(ss: array of String):TSearchComboBox;
  var
    i: integer;
  begin
      Result := TSearchComboBox.Create(nil);
      with Result as TSearchComboBox do
      begin
        Visible := False;
        Parent := Tree;
        for i:=Low(ss) to High(ss) do   Items.Add(ss[I]);
        SendMessage(GetWindow(Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
        OnKeyUp := EditKeyUp;
        OnKeyDown:=EditKeyDown;
      end;
  end;
  function MakeComboboxSL(custom: pointer; value: integer):TSearchComboBox;
  var
    i: integer;
  begin
      Result := TSearchComboBox.Create(nil);
      with Result as TSearchComboBox do
      begin
        Visible := False;
        Parent := Tree;
        Items.Assign(TStringList(custom^));
        SendMessage(GetWindow(Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
        OnKeyUp := EditKeyUp;
        OnKeyDown:=EditKeyDown;
        ItemIndex:= value;
      end;
  end;

  function MakeIntSpinEdit(min,max: integer): TTreeSpinEdit;
  begin
    Result := TTreeSpinEdit.Create(nil);
    with Result as TTreeSpinEdit do
    begin
      Visible := False;
      MinValue:=min;
      MaxValue:=max;
      Value:=SmallInt(Data.PValue^);
      Parent := Tree;
      OnKeyUp := EditKeyUp;
      OnKeyDown:= EditKeyDown;
    end;
  end;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FNumEdit := 0;
  FCheckList:= False;
  // determine what edit type actually is needed
  FEdit.Free;
  FEdit := nil;
  Data := FTree.GetNodeData(Node);
  case Data.DBType of
    SavStringRes:
      begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.DValue;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
          OnKeyPress:= KeyPress;
        end;
      end;
   SavInt:
    begin
        FEdit:= MakeIntSpinEdit(Low(SmallInt),High(SmallInt));
    end;
   SavAttribute:
    begin
        FEdit:= MakeIntSpinEdit(0,20);
    end;
   SavController:
    begin
      FEdit:= MakeIntSpinEdit(1,4);
    end;
    SavTimeStage,SavUInt:
    begin
      FEdit:= MakeIntSpinEdit(0,High(SmallInt));
    end;
   SavFloat:
      begin
        FEdit := TFloatSpinEdit.Create(nil);
        with FEdit as TFloatSpinEdit do
        begin
          Visible := False;
          Value:=Single(Data.PValue^);
          Increment:= 1.0;
          Parent := Tree;
          OnKeyUp := EditKeyUp;
          OnKeyDown:= EditKeyDown;
        end;
      end;
   SavTrainingEnum:
     begin
      FEdit := TSearchComboBox.Create(nil);
      with FEdit as TSearchComboBox do
      begin
        Visible := False;
        Parent := Tree;
        SendMessage(GetWindow(Handle,GW_CHILD), EM_SETREADONLY, 1, 0);
        OnKeyUp := EditKeyUp;
        OnKeyDown:=EditKeyDown;
        tempClassWeakness:= PClassWeakness(TData(Data.Custom^).pGhostClassName)^;
        strlist:= TStringList(TData(Data.Custom^).pUtf8Strings^);
        value:= THaunterData(Data.PValue^).gtype;
        case tempClassWeakness[value].GClass of
          1: for i:= Low(Sprites) to High(Sprites) do Items.Add(strlist[Sprites[i]]);
          2: for i:= Low(Disturbances) to High(Disturbances) do Items.Add(strlist[Disturbances[i]]);
          3: for i:= Low(Elementals) to High(Elementals) do Items.Add(strlist[Elementals[i]]);
          4: for i:= Low(Vapours) to High(Vapours) do Items.Add(strlist[Vapours[i]]);
          5: for i:= Low(Frighteners) to High(Frighteners) do Items.Add(strlist[Frighteners[i]]);
          6: for i:= Low(Horrors) to High(Horrors) do Items.Add(strlist[Horrors[i]]);
        end;
        ItemIndex:=  THaunterData(Data.PValue^).parameters.training_level;
      end;
     end;
   SavPower:
   begin
     FEdit:= MakePowerComboBox(Integer(Data.PValue^));
   end;
   SavPowerShop:
   begin
     FEdit:= MakePowerComboBox(Integer(Data.PValue^));
     p:= Data.PValue;
     Inc(PCardinal(p),1);
     FEdit2:= MakePowerComboBox(Integer(p^));
     FNumEdit:=1;
   end;
   SavMultiplier:
      begin
        FEdit := MakeCombobox(Multipliers);
        TSearchComboBox(FEdit).ItemIndex:=  TSearchComboBox(FEdit).Items.IndexOf(IntToStr(Integer(Data.PValue^)));
      end;
   SavBoolean:
      begin
        FEdit := MakeCombobox(OptBool);
        TSearchComboBox(FEdit).ItemIndex:= Integer(Data.PValue^);
      end;
   SavSaveType:
      begin
        FEdit := MakeCombobox(SaveTypes);
        TSearchComboBox(FEdit).ItemIndex:= Integer(Data.PValue^);
      end;
   SavGhostType:
       begin
      FEdit := MakeComboboxSL(TData(Data.Custom^).pGhostTypeStrings,THaunterData(Data.PValue^).gtype);
    end;
   SavGhostAnim,SavSaveState,SavStringEnum:
    begin
      FEdit := MakeComboboxSL(Data.Custom,Integer(Data.PValue^));
    end;
   SavTime:
      begin
       FEdit := TMaskEdit.Create(nil);
         with FEdit as TMaskEdit do
        begin
          Visible := False;
          Parent := Tree;
          EditMask := '99:99:99:99;1;0';
          Text := Data.DValue;
          OnKeyPress:= KeyPress;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
          OnChange:= TimeEditChange;
        end;
      end;
   SavDword:
      begin
        FEdit := TMaskEdit.Create(nil);
        with FEdit as TMaskEdit do
        begin
          Visible := False;
          Parent := Tree;
          EditMask := '>aaaaaaaa<;1;0';
          Text := Data.DValue;
          OnKeyPress:= HexEditKeyPress;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
          OnChange:= HexEditChange;
        end;
      end;
   SavFlags:
      begin
        FEdit := TCheckedComboBox.Create(nil);
        with FEdit as TCheckedComboBox do
        begin
          Visible := False;
          Parent := Tree;
          for i:=Low(FlagNames) to High(FlagNames) do Items.Add(FlagNames[I]);
          value:= Integer(Data.PValue^);
          for i:=Low(FlagNames) to High(FlagNames) do
          begin
            if IsBitSet(value,i) then
              Checked[i]:= True;
          end;
        end;
      end;
   SavFetter:
      begin
        FEdit := TCheckedComboBox.Create(nil);
        with FEdit as TCheckedComboBox do
        begin
          Visible := False;
          Parent := Tree;
          ImageList:= TImageList(TData(Data.Custom^).pFetterImgList^);
          strlist:= TStringList(TData(Data.Custom^).pUtf8Strings^);
          //add items
          Items.AddObject(strlist[1379],TObject(0));
          Items.AddObject(strlist[1381],TObject(1));
          Items.AddObject(strlist[1382],TObject(2));
          Items.AddObject(strlist[1383],TObject(3));
          Items.AddObject(strlist[1384],TObject(4));
          Items.AddObject(strlist[1385],TObject(5));
          Items.AddObject(strlist[1386],TObject(6));
          Items.AddObject(strlist[1387],TObject(7));
          Items.AddObject(strlist[1388],TObject(8));
          Items.AddObject(strlist[1389],TObject(9));
          Items.AddObject(strlist[1390],TObject(10));
          Items.AddObject(strlist[1391],TObject(11));
          Items.AddObject(strlist[1392],TObject(12));
          Items.AddObject(strlist[1393],TObject(13));
          Items.AddObject(strlist[1394],TObject(14));
          Items.AddObject(strlist[429],TObject(15));
          value:= Integer(Data.PValue^);
          for i:=0 to Items.Count -1 do
          begin
            if i = 15 then
            begin
             if IsBitSet(value,17) then
               Checked[i]:= True;
            end
            else
            if IsBitSet(value,i) then
             Checked[i]:= true;
          end;
        end;
      end;
  else
    Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);

var
  Dummy: Integer;
  RTemp: TRect;
begin
  FRect:=R;
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  case FNumEdit of
    0: begin
      FEdit.BoundsRect := R;
    end;
    1:
    begin
      RTemp:= R;
      RTemp.Right:=R.Left + ((R.Right-R.Left) div 2);
      FEdit.BoundsRect := RTemp;
      RTemp.Left:=R.Left + ((R.Right-R.Left) div 2);
      RTemp.Right:=R.Right;
      FEdit2.BoundsRect := RTemp;
    end;
  end;
end;

end.
