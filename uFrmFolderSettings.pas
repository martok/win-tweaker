unit uFrmFolderSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, StdCtrls,
  registry, Dialogs;

type
  TfrmFolderSettings = class(TFrame)
    btnResetMediaTypes: TButton;
    btnCopyMediaTypes: TButton;
    btnResetAll: TButton;
    procedure btnCopyMediaTypesClick(Sender: TObject);
    procedure btnResetAllClick(Sender: TObject);
    procedure btnResetMediaTypesClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

const
  sShellKey = '\Software\Classes\Local Settings\Software\Microsoft\Windows\Shell\';
  sShellBags = 'Bags';
  sShellBagMRU = 'BagMRU';

  sFolderTypeDefaults = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Streams\Defaults';
  sFolderTypes : array[0..4] of string = (
    '{5C4F28B5-F869-4E84-8E60-F11DB97C5CC7}',   // Generic
    '{7D49D726-3C21-4F05-99AA-FDC2C9474656}',   // Documents
    '{94D6DDCC-4A68-4175-A374-BD584A510B78}',   // Music
    '{B3690E58-E961-423B-B687-386EBFD83239}',   // Pictures
    '{5FA96407-7E77-483C-AC93-691D05850DE8}'    // Videos
  );

{$R *.lfm}

{ TfrmFolderSettings }

procedure TfrmFolderSettings.btnResetAllClick(Sender: TObject);
var
  Reg: TRegistry;
  procedure ClearKey(k: String);
  var
    l: TStringList;
    s: string;
  begin
    if reg.OpenKey(k, false) then begin
      l:= TStringList.Create;
      try
        Reg.GetKeyNames(l);
        for s in l do
          ClearKey(k + '\' + s);
      finally
        FreeAndNil(l);
      end;
    end;
    Reg.DeleteKey(k);
  end;

begin
  if not (mrYes = MessageDlg('Reset All Folder Settings', 'Reset all stored folder settings ("Bags") and use defaults?', mtConfirmation, [mbYes, mbNo],0)) then
    exit;
  Reg:= TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKey(sShellKey, false) then begin
      ClearKey(sShellKey + sShellBags);
      ClearKey(sShellKey + sShellBagMRU);
    end;
  finally
    FreeAndNil(Reg);
  end;

end;

procedure TfrmFolderSettings.btnCopyMediaTypesClick(Sender: TObject);
var
  Reg: TRegistry;
  rd: TRegDataInfo;
  buf: TMemoryStream;
  i: integer;
begin
  if not (mrYes = MessageDlg('Copy Media Types', 'Copy media setttings for "Generic" to "Documents", "Music", "Pictures", "Video" defaults?', mtConfirmation, [mbYes, mbNo],0)) then
    exit;
  Reg:= TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKey(sFolderTypeDefaults, false) then begin
      if Reg.GetDataInfo(sFolderTypes[0], rd{%H-}) then begin
        buf:= TMemoryStream.Create;
        try
          buf.SetSize(rd.DataSize);
          Reg.ReadBinaryData(sFolderTypes[0], buf.Memory^, buf.Size);
          for i:= 1 to high(sFolderTypes) do
            reg.WriteBinaryData(sFolderTypes[i], buf.Memory^, buf.Size);
        finally
          FreeAndNil(buf);
        end;
      end else begin
        for i:= 1 to high(sFolderTypes) do
          reg.DeleteValue(sFolderTypes[i]);
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
  btnResetAll.Click;
end;


procedure TfrmFolderSettings.btnResetMediaTypesClick(Sender: TObject);
var
  Reg: TRegistry;
  i: integer;
begin
  if not (mrYes = MessageDlg('Restore Media Types', 'Restore Windows defaults for "Documents", "Music", "Pictures", "Video" folders? "Generic" remains unchanged.', mtConfirmation, [mbYes, mbNo],0)) then
    exit;
  Reg:= TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKey(sFolderTypeDefaults, false) then begin
      for i:= 1 to high(sFolderTypes) do
        reg.DeleteValue(sFolderTypes[i]);
    end;
  finally
    FreeAndNil(Reg);
  end;
  btnResetAll.Click;
end;

end.

