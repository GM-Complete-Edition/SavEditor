program SavEditor_project;

uses
  Forms,
  SavEditor in 'SavEditor.pas' {Form1},
  HaunterData in 'HaunterData.pas',
  SavClass in 'SavClass.pas',
  Editors in 'Editors.pas',
  IntSpinEdit in 'IntSpinEdit.pas',
  AddHaunters in 'AddHaunters.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
