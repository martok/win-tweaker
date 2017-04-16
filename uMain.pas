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
  fileinfo,
  uFrmConsole, uFrmSysParameters, uFrmShellIcons, uFrmFolderSettings, uFrmAppKeys;

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

(*
Routine Description: This routine returns TRUE if the caller's
process is a member of the Administrators local group. Caller is NOT
expected to be impersonating anyone and is expected to be able to
open its own process and process token.
Arguments: None.
Return Value:
 TRUE - Caller has Administrators local group.
 FALSE - Caller does not have Administrators local group. --
*)

const
  SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5));

function CheckTokenMembership(TokenHandle: HANDLE; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall; external advapi32;

function IsUserAdmin: BOOL;
var
  NTAuthority: SID_IDENTIFIER_AUTHORITY;
  AdministratorsGroup: PSID;
begin
  Result:= false;
  NTAuthority:= SECURITY_NT_AUTHORITY;
  AdministratorsGroup:= nil;
  Result:= AllocateAndInitializeSid(@NTAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0,0,0,0,0,0, AdministratorsGroup);
  if Result then begin
    if not CheckTokenMembership(0, AdministratorsGroup, Result) then
      Result:= false;
    FreeSid(AdministratorsGroup);
  end;
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

