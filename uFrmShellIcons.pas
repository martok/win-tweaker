unit uFrmShellIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, ComCtrls, registry,
  uCInitFrame, windows, Graphics, StdCtrls, Dialogs;

type
  TfrmShellIcons = class(TFrame, IInitializable)
    btnReload: TBitBtn;
    btnSaveChanges: TBitBtn;
    cbViewStyle: TComboBox;
    ilShellIcons: TImageList;
    ilShellIconsSm: TImageList;
    lvIcons: TListView;
    Panel1: TPanel;
    procedure btnReloadClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure cbViewStyleSelect(Sender: TObject);
    procedure lvIconsDblClick(Sender: TObject);
  private
    { private declarations }
    function AddIcons(small, large: HICON): Integer;
  public
    { public declarations }
    procedure Initialize;
  end;

implementation

uses
  uCStockIconInfo;

type
  TIconDef = record
    ID: SHSTOCKICONID;
    Group,
    Descr: String;
  end;

const
  sShellIconsRegKey = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Icons';
  idIcons: array[0..92] of TIconDef = (
    (ID: SIID_DOCNOASSOC;          Group: 'Explorer'; Descr: 'Unknown File Type'),
    (ID: SIID_DOCASSOC;            Group: 'Explorer'; Descr: 'Default Document'),
    (ID: SIID_APPLICATION;         Group: 'Explorer'; Descr: 'Default Application'),
    (ID: SIID_FOLDER;              Group: 'Explorer'; Descr: 'Folder (Closed)'),
    (ID: SIID_FOLDEROPEN;          Group: 'Explorer'; Descr: 'Folder (Open)'),
    (ID: SIID_DRIVE525;            Group: 'Drive';    Descr: '5.25" Floppy'),
    (ID: SIID_DRIVE35;             Group: 'Drive';    Descr: '3.5" Floppy'),
    (ID: SIID_DRIVEREMOVE;         Group: 'Drive';    Descr: 'Removable Drive'),
    (ID: SIID_DRIVEFIXED;          Group: 'Drive';    Descr: 'Hard Drive'),
    (ID: SIID_DRIVENET;            Group: 'Drive';    Descr: 'Network Drive'),
    (ID: SIID_DRIVENETDISABLED;    Group: 'Drive';    Descr: 'Network Drive (disconnected)'),
    (ID: SIID_DRIVECD;             Group: 'Drive';    Descr: 'CD-ROM Drive'),
    (ID: SIID_DRIVERAM;            Group: 'Drive';    Descr: 'RAM Disk'),
    (ID: SIID_WORLD;               Group: 'Global';   Descr: 'Entire Network'),
    (ID: SIID_SERVER;              Group: 'Global';   Descr: 'Networked Computer'),
    (ID: SIID_PRINTER;             Group: 'Printer';  Descr: 'Printer'),
    (ID: SIID_MYNETWORK;           Group: 'Desktop';  Descr: 'Network Neighborhood'),
    (ID: SIID_FIND;                Group: '';         Descr: 'Find'),
    (ID: SIID_HELP;                Group: '';         Descr: 'Help'),
    (ID: SIID_SHARE;               Group: 'Explorer'; Descr: 'Share'),
    (ID: SIID_LINK;                Group: '';         Descr: 'Link'),
    (ID: SIID_SLOWFILE;            Group: '';         Descr: 'Slow file'),
    (ID: SIID_RECYCLER;            Group: '';         Descr: 'Recycler (empty)'),
    (ID: SIID_RECYCLERFULL;        Group: '';         Descr: 'Recycler (full)'),
    (ID: SIID_MEDIACDAUDIO;        Group: '';         Descr: 'Audio CD'),
    (ID: SIID_LOCK;                Group: '';         Descr: 'Lock'),
    (ID: SIID_AUTOLIST;            Group: '';         Descr: 'Auto List'),
    (ID: SIID_PRINTERNET;          Group: 'Printer';  Descr: 'Printer (network)'),
    (ID: SIID_SERVERSHARE;         Group: 'Explorer'; Descr: 'Server Share'),
    (ID: SIID_PRINTERFAX;          Group: 'Printer';  Descr: 'Fax'),
    (ID: SIID_PRINTERFAXNET;       Group: 'Printer';  Descr: 'Fax (network)'),
    (ID: SIID_PRINTERFILE;         Group: 'Printer';  Descr: 'File Printer'),
    (ID: SIID_STACK;               Group: '';         Descr: 'Stack'),
    (ID: SIID_MEDIASVCD;           Group: '';         Descr: 'SVCD'),
    (ID: SIID_STUFFEDFOLDER;       Group: '';         Descr: 'Stuffed Folder'),
    (ID: SIID_DRIVEUNKNOWN;        Group: 'Drive';    Descr: 'Unknown Drive type'),
    (ID: SIID_DRIVEDVD;            Group: 'Drive';    Descr: 'DVD Drive'),
    (ID: SIID_MEDIADVD;            Group: 'Media';    Descr: 'DVD'),
    (ID: SIID_MEDIADVDRAM;         Group: 'Media';    Descr: 'DVD-RAM'),
    (ID: SIID_MEDIADVDRW;          Group: 'Media';    Descr: 'DVD-RW'),
    (ID: SIID_MEDIADVDR;           Group: 'Media';    Descr: 'DVD-R'),
    (ID: SIID_MEDIADVDROM;         Group: 'Media';    Descr: 'DVD-ROM'),
    (ID: SIID_MEDIACDAUDIOPLUS;    Group: 'Media';    Descr: 'Audio CD Plus'),
    (ID: SIID_MEDIACDRW;           Group: 'Media';    Descr: 'CD-RW'),
    (ID: SIID_MEDIACDR;            Group: 'Media';    Descr: 'CD-R'),
    (ID: SIID_MEDIACDBURN;         Group: 'Media';    Descr: 'Burn CD'),
    (ID: SIID_MEDIABLANKCD;        Group: 'Media';    Descr: 'Blank CD'),
    (ID: SIID_MEDIACDROM;          Group: 'Media';    Descr: 'CD-ROM'),
    (ID: SIID_AUDIOFILES;          Group: 'Folder';   Descr: 'Audio'),
    (ID: SIID_IMAGEFILES;          Group: 'Folder';   Descr: 'Image'),
    (ID: SIID_VIDEOFILES;          Group: 'Folder';   Descr: 'Video'),
    (ID: SIID_MIXEDFILES;          Group: 'Folder';   Descr: 'Mixed'),
    (ID: SIID_FOLDERBACK;          Group: 'Folder';   Descr: 'Folder Back'),
    (ID: SIID_FOLDERFRONT;         Group: 'Folder';   Descr: 'Folder Front'),
    (ID: SIID_SHIELD;              Group: 'Global';   Descr: 'Shield'),
    (ID: SIID_WARNING;             Group: 'Global';   Descr: 'Warning'),
    (ID: SIID_INFO;                Group: 'Global';   Descr: 'Info'),
    (ID: SIID_ERROR;               Group: 'Global';   Descr: 'Error'),
    (ID: SIID_KEY;                 Group: '';         Descr: 'Key'),
    (ID: SIID_SOFTWARE;            Group: '';         Descr: 'Software'),
    (ID: SIID_RENAME;              Group: '';         Descr: 'Rename'),
    (ID: SIID_DELETE;              Group: '';         Descr: 'Delete'),
    (ID: SIID_MEDIAAUDIODVD;       Group: 'Media';    Descr: 'DVD-Audio'),
    (ID: SIID_MEDIAMOVIEDVD;       Group: 'Media';    Descr: 'DVD-Video'),
    (ID: SIID_MEDIAENHANCEDCD;     Group: 'Media';    Descr: 'CD-Enhanced'),
    (ID: SIID_MEDIAENHANCEDDVD;    Group: 'Media';    Descr: 'DVD-Enhanced'),
    (ID: SIID_MEDIAHDDVD;          Group: 'Media';    Descr: 'HD-DVD'),
    (ID: SIID_MEDIABLURAY;         Group: 'Media';    Descr: 'Bluray'),
    (ID: SIID_MEDIAVCD;            Group: 'Media';    Descr: 'VCD'),
    (ID: SIID_MEDIADVDPLUSR;       Group: 'Media';    Descr: 'DVD+R'),
    (ID: SIID_MEDIADVDPLUSRW;      Group: 'Media';    Descr: 'DVD+RW'),
    (ID: SIID_DESKTOPPC;           Group: '';         Descr  : 'Desktop PC'),
    (ID: SIID_MOBILEPC;            Group: '';         Descr: 'Mobile PC'),
    (ID: SIID_USERS;               Group: '';         Descr: 'Users'),
    (ID: SIID_MEDIASMARTMEDIA;     Group: 'Media';    Descr: 'SmartMedia'),
    (ID: SIID_MEDIACOMPACTFLASH;   Group: 'Media';    Descr: 'CompactFlash'),
    (ID: SIID_DEVICECELLPHONE;     Group: 'Device';   Descr: 'Cellphone'),
    (ID: SIID_DEVICECAMERA;        Group: 'Device';   Descr: 'Camera'),
    (ID: SIID_DEVICEVIDEOCAMERA;   Group: 'Device';   Descr: 'Videocamera'),
    (ID: SIID_DEVICEAUDIOPLAYER;   Group: 'Device';   Descr: 'Audioplayer'),
    (ID: SIID_NETWORKCONNECT;      Group: '';         Descr: 'Network connect'),
    (ID: SIID_INTERNET;            Group: '';         Descr: 'Internet'),
    (ID: SIID_ZIPFILE;             Group: '';         Descr: 'ZIP-File'),
    (ID: SIID_SETTINGS;            Group: '';         Descr: 'Settings'),
    (ID: SIID_DRIVEHDDVD;          Group: 'Drive';    Descr: 'HD-DVD Drive'),
    (ID: SIID_DRIVEBD;             Group: 'Drive';    Descr: 'Bluray Drive'),
    (ID: SIID_MEDIAHDDVDROM;       Group: 'Media';    Descr: 'HD-DVD-ROM'),
    (ID: SIID_MEDIAHDDVDR;         Group: 'Media';    Descr: 'HD-DVD-R'),
    (ID: SIID_MEDIAHDDVDRAM;       Group: 'Media';    Descr: 'HD-DVD-RAM'),
    (ID: SIID_MEDIABDROM;          Group: 'Media';    Descr: 'BD-ROM'),
    (ID: SIID_MEDIABDR;            Group: 'Media';    Descr: 'BD-R'),
    (ID: SIID_MEDIABDRE;           Group: 'Media';    Descr: 'BD-RE'),
    (ID: SIID_CLUSTEREDDRIVE;      Group: 'Drive';    Descr: 'Clustered')
    //(ID: SIID_MAX_ICONS          = 175                  Group: ''),
  );

{$R *.lfm}

{ TfrmShellIcons }

type
  PHICON = ^HICON;

function ExtractIconEx(lpszFile: LPCSTR; nIconIndex: Integer; phIconLarge, phIconSmall: PHICON; nIcons: UINT):UINT; external 'shell32' name 'ExtractIconExA';
function ExtractIconExW(lpszFile: LPCWSTR; nIconIndex: Integer; phIconLarge, phIconSmall: PHICON; nIcons: UINT):UINT; external 'shell32' name 'ExtractIconExW';
function PathUnExpandEnvStrings(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):BOOL; external 'shlwapi' name 'PathUnExpandEnvStringsA';

function ExpandEnvironment(const aStr: String): String;
var
  f: AnsiString;
begin
  SetLength(f, ExpandEnvironmentStringsA(PAnsiChar(aStr), nil, 0));
  SetLength(f, ExpandEnvironmentStringsA(PAnsiChar(aStr), PAnsiChar(f), Length(f)));
  Result:= f;
end;

function UnExpandEnvironment(const aStr: String): String;
var
  f: AnsiString;
begin
  SetLength(f, MAX_PATH + 5);
  if PathUnExpandEnvStrings(PAnsiChar(aStr), PAnsiChar(f), Length(f)) then
    Result:= f
  else
    Result:= aStr;
end;

procedure SplitIconResDescr(const aName: string; out aFile: String; out aIconId: integer);
var
  t: integer;
begin
  t:= LastDelimiter(',',aName);
  if t > 0 then begin
    aFile:= Copy(aName, 1, t-1);
    aIconId:= StrToInt(Copy(aName, t+1, MaxInt));
  end else begin
    aFile:= aName;
    aIconId:= 0;
  end;
end;

function ExtractIcon(const aFile: string; out aLarge, aSmall: HICON): boolean;
var
  f,fn: string;
  r, n: integer;
begin
  aLarge:= 0;
  aSmall:= 0;
  f:= ExpandEnvironment(aFile);
  SplitIconResDescr(f, fn, n);

  r:= ExtractIconEx(PAnsiChar(fn), n, @aLarge, @aSmall, 1);
  Result:= r in [1, 2]; // at least one icon is valid
end;

procedure TfrmShellIcons.btnReloadClick(Sender: TObject);
var
  Reg: TRegistry;
  i: integer;
  id: TIconDef;
  iid: String;
  icofil: String;
  icl, ics: HICON;
  it: TListItem;
begin
  lvIcons.Clear;
  ilShellIcons.Clear;
  ilShellIcons.Width:= GetSystemMetrics(SM_CXICON);
  ilShellIcons.Height:= GetSystemMetrics(SM_CYICON);
  ilShellIconsSm.Clear;
  ilShellIconsSm.Width:= GetSystemMetrics(SM_CXSMICON);
  ilShellIconsSm.Height:= GetSystemMetrics(SM_CYSMICON);
  Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    if not Reg.OpenKeyReadOnly(sShellIconsRegKey) then
      FreeAndNil(Reg);
    for i:= 0 to high(idIcons) do begin
      id:= idIcons[i];
      iid:= IntToStr(Ord(id.ID));
      it:= lvIcons.Items.Add;
      it.Data:= @idIcons[i];
      it.Caption:= id.Descr;
      it.SubItems.Add(iid);
      it.SubItems.Add(id.Group);
      if Assigned(Reg) and Reg.ValueExists(iid) then begin
        icofil:= Reg.ReadString(iid);
        it.SubItems.Add(icofil);
      end
      else begin
        icofil:= GetActualStockIconPath(id.ID);
        it.SubItems.Add('');
      end;
      if ExtractIcon(icofil, icl, ics) then begin
        it.ImageIndex:= AddIcons(ics, icl);
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmShellIcons.btnSaveChangesClick(Sender: TObject);
var
  Reg: TRegistry;
  i: integer;
  it: TListItem;
  id: TIconDef;
  iid, f: string;
begin
  Reg:= TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(sShellIconsRegKey, true) then begin
      for i:= 0 to lvIcons.Items.Count-1 do begin
        it:= lvIcons.Items[i];
        id:= TIconDef(it.Data^);
        iid:= IntToStr(Ord(id.ID));
        if it.SubItems[2]>'' then begin
          f:= UnExpandEnvironment(it.SubItems[2]);
          Reg.WriteExpandString(iid, f);
        end else
          Reg.DeleteValue(iid);
      end;
    end else
      MessageDlg('Could not open registry key for writing, did you try to change global settings as non-administrator?', mtError, [mbOK], 0)
  finally
    FreeAndNil(Reg);
  end;
  btnReload.Click;
end;

procedure TfrmShellIcons.cbViewStyleSelect(Sender: TObject);
begin
  lvIcons.LargeImages:= nil;
  lvIcons.SmallImages:= nil;
  case cbViewStyle.ItemIndex of
    0: lvIcons.ViewStyle:= vsIcon;
    1: lvIcons.ViewStyle:= vsList;
    2: lvIcons.ViewStyle:= vsReport;
  end;
  lvIcons.LargeImages:= ilShellIcons;
  lvIcons.SmallImages:= ilShellIconsSm;
end;

procedure TfrmShellIcons.lvIconsDblClick(Sender: TObject);
var
  fs: string;
begin
  if Assigned(lvIcons.Selected) then begin
    fs:= lvIcons.Selected.SubItems[2];
    if InputQuery('Set new Override', 'Enter Filename', fs) then
      lvIcons.Selected.SubItems[2]:= fs;
  end;
end;

function TfrmShellIcons.AddIcons(small, large: HICON): Integer;
var
  ic: TIcon;
begin
  ic:= TIcon.Create;
  try
    ic.Handle:= small;
    ilShellIconsSm.AddIcon(ic);
  finally
    FreeAndNil(ic);
  end;

  ic:= TIcon.Create;
  try
    ic.Handle:= large;
    Result:= ilShellIcons.AddIcon(ic);
  finally
    FreeAndNil(ic);
  end;
end;

procedure TfrmShellIcons.Initialize;
begin
  btnReload.Click;
  cbViewStyle.ItemIndex:= 0;
  cbViewStyleSelect(Self);
end;

end.

