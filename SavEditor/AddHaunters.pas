unit AddHaunters;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckComboBox,SavClass;

type
  TForm2 = class(TForm)
    grp1: TGroupBox;
    cbb1: TCheckComboBox;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure WndProc(var Msg: TMessage);message WM_UPDATEUISTATE;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses SavEditor;

{$R *.dfm}

procedure TForm2.WndProc(var Msg: TMessage);
begin
 btn1.Refresh;
 btn2.Refresh;
end;

procedure TForm2.btn2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.btn3Click(Sender: TObject);
begin
  cbb1.SetAll(True);
end;

procedure TForm2.btn4Click(Sender: TObject);
begin
  cbb1.SetAll(False);
end;

procedure TForm2.btn1Click(Sender: TObject);
var
  i,numAdded,numReplaced: integer;
  sListCurrent: TStringList;
  replaceAll: boolean;
  declineAll: boolean;
  result: integer;
  cGhost: integer;
begin
  sListCurrent:= TStringList.Create;
  numAdded:= 0;
  numReplaced:= 0;
  replaceAll:= false;
  declineAll:= false;
  for i:=0 to High(Form1.cSavFile.ghosts) do
  begin
    sListCurrent.Add(Form1.cSavFile.ghosts[i].hData.model_file);
  end;
  for i:= 0 to cbb1.Items.Count -1 do
  begin
    if cbb1.Checked[i] then
    begin
      cGhost:= sListCurrent.IndexOf(cbb1.Items[i]);
      if cGhost = -1 then
      begin
        Form1.cSavFile.AddElement(i, False, -1);
        Inc(numAdded);
      end
      else
      begin
         if not declineAll then
         begin
           if replaceAll then
           begin
            //find node to replace
            Form1.cSavFile.AddElement(i, True, cGhost);
            Inc(numReplaced)
           end
           else
           begin
             result:= Form1.MyMessageDlg('Do you want to replace haunter: ' + cbb1.Items[i] + ' ?' , mtInformation, [mbYes,mbNo,mbNoToAll,mbYesToAll],['Yes','No', 'No to All', 'Yes to All'], 'Save File Editor ' + VERSION);
             case result of
               MrYes:
               begin
                  Form1.cSavFile.AddElement(i, True, cGhost);
                  Inc(numReplaced);
               end;
               mrYesToAll:
               begin
                 Form1.cSavFile.AddElement(i, True, cGhost);
                 Inc(numReplaced);
                 replaceAll:= true;

               end;
               mrNo:
               begin
               end;
               mrNoToAll:
               begin
                 declineAll:= true;
               end;
              end;
            end;
        end;
      end;
    end;
  end;
  if numAdded > 0 then
  begin
    if numReplaced > 0 then
    begin
       Form1.MyMessageDlg(IntToStr(numAdded) + ' haunters has been added and ' + IntToStr(numReplaced) + ' has been replaced' , mtInformation, [mbOK],['Ok'], 'Save File Editor ' + VERSION);
    end
    else
      Form1.MyMessageDlg(IntToStr(numAdded) + ' haunters has been added' , mtInformation, [mbOK],['Ok'], 'Save File Editor ' + VERSION);
  end;
end;

end.
