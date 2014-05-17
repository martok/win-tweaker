unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, uFrmConsole, uFrmSysParameters;

type
  TForm1 = class(TForm)
    Frame1_1: TfrmConsole;
    Frame2_1: TfrmSysParameters;
    PageControl1: TPageControl;
    tsSystemParameters: TTabSheet;
    tsConsole: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:= Application.Title;
end;

end.

