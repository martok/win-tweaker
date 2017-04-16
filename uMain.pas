unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, uCInitFrame, Windows;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    lbVersion: TLabel;
    PageControl1: TPageControl;
    tsAbout: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure AddFeaturePage(const aLabel: String; const aClass: TFrameClass);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  fileinfo, uwinImports,
  uFrmConsole, uFrmSysParameters, uFrmShellIcons, uFrmFolderSettings, uFrmAppKeys;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:= Application.Title;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: integer;
  ts: TTabSheet;
  vq: TVersionQuad;
begin
  if GetProgramVersion(vq{%H-}) then
    lbVersion.Caption:= VersionQuadToStr(vq);
  {$IFDEF WIN32}
  lbVersion.Caption:= lbVersion.Caption + sLineBreak + ' 32-bit';
  if IsWow64 then begin
    MessageDlg('Program is running as a 32bit process on a 64bit system. Some settings will not work, use the 64bit executable!', mtWarning, [mbOK], 0);
    Caption:= Caption + '  (WOW64)';
    lbVersion.Caption:= lbVersion.Caption + ' (WOW64)';
  end;
  {$ENDIF}
  {$IFDEF WIN64}
  lbVersion.Caption:= lbVersion.Caption + sLineBreak + ' 64-bit';
  {$ENDIF}
  if IsUserAdmin then lbVersion.Caption:= lbVersion.Caption + sLineBreak + 'Admin';

  AddFeaturePage('Console', TfrmConsole);
  AddFeaturePage('SPI', TfrmSysParameters);
  AddFeaturePage('Shell Icons', TfrmShellIcons);
  AddFeaturePage('Folder Settings', TfrmFolderSettings);
  AddFeaturePage('App Keys', TfrmAppKeys);

  PageControl1.ActivePageIndex:= 0;
end;

procedure TForm1.AddFeaturePage(const aLabel: String; const aClass: TFrameClass);
var
  pg: TTabSheet;
  inst: TFrame;
  init: IInitializable;
begin
  pg:= TTabSheet.Create(PageControl1);
  pg.Caption:= aLabel;
  pg.PageControl:= PageControl1;
  inst:= aClass.Create(pg);
  inst.Align:= alClient;
  inst.Parent:= pg;
  if Supports(inst, IInitializable, init) then
    init.Initialize;
end;


end.

