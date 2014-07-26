unit uFrmAppKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, Buttons,
  StdCtrls, Menus, Registry, uCInitFrame, Dialogs;

type
  TfrmAppKeys = class(TFrame, IInitializable)
    gbActive: TGroupBox;
    lvDefinedKeys: TListView;
    lvValues: TListView;
    miDelete: TMenuItem;
    miValueUser: TMenuItem;
    miValueGlobal: TMenuItem;
    pmValues: TPopupMenu;
    pmDefinedKeys: TPopupMenu;
    procedure lvDefinedKeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miDeleteClick(Sender: TObject);
    procedure miValueChange(Sender: TObject);
    procedure pmDefinedKeysPopup(Sender: TObject);
    procedure pmValuesPopup(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Initialize;
  end;

implementation

const
  Names: array[1..34] of record
    ID: Integer;
    Name: String;
  end = (
    (ID:1; Name: 'Back (Internet browser)'),
    (ID:2; Name: 'Forward (Internet browser)'),
    (ID:3; Name: 'Refresh (Internet browser)'),
    (ID:4; Name: 'Stop (Internet browser)'),
    (ID:5; Name: 'Search'),
    (ID:6; Name: 'Favorites'),
    (ID:7; Name: 'WebHome'),
    (ID:8; Name: 'Mute volume'),
    (ID:15; Name: 'Mail'),
    (ID:16; Name: 'Media'),
    (ID:17; Name: 'My Computer'),
    (ID:18; Name: 'Calculator'),
    (ID:24; Name: 'Mute microphone'),
    (ID:25; Name: 'Lower microphone volume'),
    (ID:26; Name: 'Raise microphone volume'),
    (ID:27; Name: 'Help'),
    (ID:28; Name: 'Find'),
    (ID:29; Name: 'New'),
    (ID:30; Name: 'Open'),
    (ID:31; Name: 'Close'),
    (ID:32; Name: 'Save'),
    (ID:33; Name: 'Print'),
    (ID:34; Name: 'Undo'),
    (ID:35; Name: 'Redo'),
    (ID:36; Name: 'Copy'),
    (ID:37; Name: 'Cut'),
    (ID:38; Name: 'Paste'),
    (ID:39; Name: 'Reply'),
    (ID:40; Name: 'Forward (mail)'),
    (ID:41; Name: 'Send'),
    (ID:42; Name: 'Spelling checker'),
    (ID:43; Name: 'Toggle dictation and'),
    (ID:44; Name: 'Toggle microphone'),
    (ID:45; Name: 'Corrections')
  );
  sAppKeysRegKey = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\AppKey';
  sRegNameShell = 'ShellExecute';
  sRegNameAssociation = 'Association';
  sRegNameRegisteredApp = 'RegisteredApp';

{$R *.lfm}

function NameFromID(id: integer): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to High(Names) do
    if Names[i].ID = id then
      Exit(Names[i].Name);
end;

{ TfrmAppKeys }

procedure TfrmAppKeys.Initialize;
var
  Reg: TRegistry;
  keys: TStringList;
  i,id: integer;
begin
  lvDefinedKeys.Clear;
  lvDefinedKeys.SortType:= stNone;
  Reg:= TRegistry.Create(KEY_READ);
  keys:= TStringList.Create;
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(sAppKeysRegKey) then begin
      Reg.GetKeyNames(keys);
      for i:= 0 to keys.Count - 1 do begin
        if TryStrToInt(keys[i], id) then begin
          with lvDefinedKeys.Items.Add do begin
            Caption:=keys[i];
            SubItems.Add(NameFromID(id));
          end;
        end;
      end;
    end;
    Reg.RootKey:= HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(sAppKeysRegKey) then begin
      Reg.GetKeyNames(keys);
      for i:= 0 to keys.Count - 1 do begin
        if TryStrToInt(keys[i], id) then begin
          if lvDefinedKeys.Items.FindCaption(0, IntToStr(id), false, true, false) = nil then
            with lvDefinedKeys.Items.Add do begin
              Caption:=keys[i];
              SubItems.Add(NameFromID(id));
            end;
        end;
      end;
    end;
    lvDefinedKeys.SortType:= stText;
  finally
    FreeAndNil(keys);
    FreeAndNil(Reg);
  end;
  lvDefinedKeysSelectItem(Self, nil, false);
end;

procedure TfrmAppKeys.lvDefinedKeysSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Reg: TRegistry;
  shell, assoc, regapp: TListItem;

  procedure AddItem(P: Boolean; item: TListItem; K: string);
  begin
    if P and Reg.ValueExists(K) then
      item.SubItems.Add(Reg.ReadString(K))
    else
      item.SubItems.Add('');
  end;

  procedure AddItems;
  var
    f: boolean;
  begin
    f:= Reg.OpenKeyReadOnly(sAppKeysRegKey + '\' + Item.Caption);
    AddItem(f, shell, sRegNameShell);
    AddItem(f, assoc, sRegNameAssociation);
    AddItem(f, regapp, sRegNameRegisteredApp);
  end;

begin
  gbActive.Enabled:= Selected;
  gbActive.Caption:= '';
  lvValues.Clear;
  if Selected then begin
    gbActive.Caption:= Item.Caption + ': ' + Item.SubItems[0];
    shell:= lvValues.Items.Add;
    assoc:= lvValues.Items.Add;
    regapp:= lvValues.Items.Add;
    shell.Caption:= sRegNameShell;
    assoc.Caption:= sRegNameAssociation;
    regapp.Caption:= sRegNameRegisteredApp;
    Reg:= TRegistry.Create(KEY_READ);
    try
      Reg.RootKey:= HKEY_LOCAL_MACHINE;
      AddItems;

      Reg.RootKey:= HKEY_CURRENT_USER;
      AddItems;
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

procedure TfrmAppKeys.pmValuesPopup(Sender: TObject);
begin
  miValueGlobal.Enabled:= lvValues.Selected<>nil;
  miValueUser.Enabled:= lvValues.Selected<>nil;
end;

procedure TfrmAppKeys.miValueChange(Sender: TObject);
var
  Reg: TRegistry;
  k, v: string;
begin
  k:= lvValues.Selected.Caption;
  Reg:= TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Sender = miValueGlobal then
      Reg.RootKey:= HKEY_LOCAL_MACHINE
    else
      Reg.RootKey:= HKEY_CURRENT_USER;

    if Reg.OpenKey(sAppKeysRegKey + '\' + lvDefinedKeys.Selected.Caption, True) then begin
      v:= '';
      if Reg.ValueExists(k) then
        v:= Reg.ReadString(v);
      if InputQuery('Edit '+k,'(leave blank to reset to default)',v) then begin
        if v > '' then
          Reg.WriteString(k, v)
        else
          Reg.DeleteValue(k);
      end;
    end else
      MessageDlg('Could not open registry key for writing, did you try to change global settings as non-administrator?', mtError, [mbOK], 0);
  finally
    FreeAndNil(Reg);
  end;
  lvDefinedKeysSelectItem(Self, lvDefinedKeys.Selected, true);
end;

procedure TfrmAppKeys.pmDefinedKeysPopup(Sender: TObject);
begin
  miDelete.Enabled:= lvDefinedKeys.Selected<>nil;
end;

procedure TfrmAppKeys.miDeleteClick(Sender: TObject);
var
  Reg: TRegistry;
  k, s: string;
  lm: boolean;
begin
  k:= lvDefinedKeys.Selected.Caption;
  if MessageDlg(Format('Delete AppKey Definition for %s?', [k]), mtConfirmation, mbYesNo, 0) = mrYes then begin
    Reg:= TRegistry.Create(KEY_ALL_ACCESS);
    try
      lm:= false;
      Reg.RootKey:= HKEY_CURRENT_USER;
      if Reg.OpenKey(sAppKeysRegKey, false) and reg.KeyExists(k) then
        Reg.DeleteKey(k);

      Reg.RootKey:= HKEY_LOCAL_MACHINE;
      if Reg.OpenKey(sAppKeysRegKey, false) and reg.KeyExists(k) then begin
        Reg.DeleteKey(k);
        lm:= true;
      end;
      s:= 'Deleted AppKey definition for user';
      if lm then
        s:= s + ' and globally';
      s:= s + '.';
      ShowMessage(s);
    finally
      FreeAndNil(Reg);
    end;
    Initialize;
  end;
end;

end.

