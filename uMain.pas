unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, uFrmConsole, uFrmSysParameters, uFrmShellIcons,
  uFrmFolderSettings, Windows;

type
  TForm1 = class(TForm)
    Frame1_1: TfrmConsole;
    Frame2_1: TfrmSysParameters;
    frmFolderSettings1: TfrmFolderSettings;
    frmShellIcons1: TfrmShellIcons;
    Label1: TLabel;
    lbVersion: TLabel;
    PageControl1: TPageControl;
    tsAbout: TTabSheet;
    tsFolderSettings: TTabSheet;
    tsShellIcons: TTabSheet;
    tsSystemParameters: TTabSheet;
    tsConsole: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uCInitFrame, fileinfo;

{$R *.lfm}

function IsWow64: Boolean;
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: Windows.THandle; var Res: Windows.BOOL
  ): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL;      // Result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  IsWow64Result:= false;
  // Try to load required function from kernel32
  IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'
  ));
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(
      Windows.GetCurrentProcess, IsWow64Result
    ) then
      raise SysUtils.Exception.Create('IsWow64: bad process handle');
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:= Application.Title;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: integer;
  ts: TTabSheet;
  init: IInitializable;
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

  for i:= 0 to PageControl1.PageCount - 1 do begin
    ts:= PageControl1.Pages[i];
    if (ts.ControlCount>0) and Supports(ts.Controls[0], IInitializable, init) then
      init.Initialize;
  end;
  PageControl1.ActivePageIndex:= 0;
end;


end.

