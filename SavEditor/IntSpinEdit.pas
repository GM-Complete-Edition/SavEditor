unit IntSpinEdit;

interface

uses Spin;

type
  TTreeSpinEdit = class(TSpinEdit)
  procedure KeyPress(var Key: Char); override;
end;
implementation
procedure TTreeSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

end.
