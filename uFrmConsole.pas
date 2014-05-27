unit uFrmConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uCInitFrame, Spin, Dialogs, Buttons, registry;

type
  TfrmConsole = class(TFrame, IInitializable)
    Bevel1: TBevel;
    btnDeleteWindowKey: TBitBtn;
    btnSaveChanges: TBitBtn;
    cbFontSizeFix: TComboBox;
    cbNoDuplicates: TCheckBox;
    cbStoreQuickedit: TCheckBox;
    cbStoreInsertMode: TCheckBox;
    cbStoreHistoryLen: TCheckBox;
    cbStoreHistoryNum: TCheckBox;
    cbStoreHistoryDup: TCheckBox;
    cbStoreFont: TCheckBox;
    cbStoreWinSize: TCheckBox;
    cbStoreScrBufSize: TCheckBox;
    cbStoreWinPos: TCheckBox;
    cbStoreWinposAuto: TCheckBox;
    cbWindowNameSel: TComboBox;
    cbCursorSize: TComboBox;
    cbQuickEdit: TCheckBox;
    cbInsertMode: TCheckBox;
    cbAutomatic: TCheckBox;
    cbStoreCursorSize: TCheckBox;
    cbFontFace: TComboBox;
    cbFontBold: TCheckBox;
    cbFontSizeVec: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    seHistoryLength: TSpinEdit;
    seHistoryBufferCount: TSpinEdit;
    seWinPosX: TSpinEdit;
    seWinPosY: TSpinEdit;
    seWinSizeX: TSpinEdit;
    seScrBufSizeX: TSpinEdit;
    seWinSizeY: TSpinEdit;
    seScrBufSizeY: TSpinEdit;
    procedure btnDeleteWindowKeyClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure cbFontFaceSelect(Sender: TObject);
    procedure cbStoreChecked(Sender: TObject);
    procedure cbWindowNameSelChange(Sender: TObject);
    procedure seScrBufChange(Sender: TObject);
    procedure seWinSizeChange(Sender: TObject);
  private
    { private declarations }
    fCodeChange: boolean;
    procedure ReadItems;
    procedure EnableControls;
  public
    { public declarations }
    procedure Initialize;
  end;

implementation

uses
  Math;

const
  sConsoleRegKey = '\Console';

resourcestring
  sGlobalConfig = '(Global)';

{$R *.lfm}

function MakeLongWord(h,l: Word): integer;
begin
  Result:= (h shl 16) or l;
end;

{ TfrmConsole }

procedure TfrmConsole.seScrBufChange(Sender: TObject);
begin
  if fCodeChange then exit;
  fCodeChange:= true;
  try
    seWinSizeX.Value:= min(seWinSizeX.Value, seScrBufSizeX.Value);
    seWinSizeY.Value:= min(seWinSizeY.Value, seScrBufSizeY.Value);
  finally
    fCodeChange:= false;
  end;
end;

procedure TfrmConsole.seWinSizeChange(Sender: TObject);
begin
  if fCodeChange then exit;
  fCodeChange:= true;
  try
    seScrBufSizeX.Value:= max(seWinSizeX.Value, seScrBufSizeX.Value);
    seScrBufSizeY.Value:= max(seWinSizeY.Value, seScrBufSizeY.Value);
  finally
    fCodeChange:= false;
  end;
end;

procedure TfrmConsole.ReadItems;
var
  Reg: TRegistry;
begin
  Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey(sConsoleRegKey, false);
    cbWindowNameSel.Clear;
    Reg.GetKeyNames(cbWindowNameSel.Items);
    cbWindowNameSel.Items.Insert(0, sGlobalConfig);
    cbWindowNameSel.ItemIndex:= 0;
    cbWindowNameSelChange(Self);
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmConsole.Initialize;
begin
  ReadItems;
end;

procedure TfrmConsole.cbWindowNameSelChange(Sender: TObject);
var
  Reg: TRegistry;
  ws: DWord;
begin
  btnDeleteWindowKey.Enabled:= cbWindowNameSel.ItemIndex > 0;

  Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    case cbWindowNameSel.ItemIndex of
      0: Reg.OpenKey(sConsoleRegKey, false);
    else
      Reg.OpenKey(sConsoleRegKey + '\' +  cbWindowNameSel.Text, false);
    end;

    cbStoreCursorSize.Checked:= Reg.ValueExists('CursorSize');
    if cbStoreCursorSize.Checked then cbCursorSize.Text:= IntToStr(Reg.ReadInteger('CursorSize'));
    cbStoreQuickedit.Checked:= Reg.ValueExists('QuickEdit');
    if cbStoreQuickedit.Checked then cbQuickEdit.Checked:= Reg.ReadBool('QuickEdit');
    cbStoreInsertMode.Checked:= Reg.ValueExists('InsertMode');
    if cbStoreInsertMode.Checked then cbInsertMode.Checked:= Reg.ReadBool('InsertMode');
    cbStoreHistoryLen.Checked:= Reg.ValueExists('HistoryBufferSize');
    if cbStoreHistoryLen.Checked then seHistoryLength.Value:= Reg.ReadInteger('HistoryBufferSize');
    cbStoreHistoryNum.Checked:= Reg.ValueExists('NumberOfHistoryBuffers');
    if cbStoreHistoryNum.Checked then seHistoryBufferCount.Value:= Reg.ReadInteger('NumberOfHistoryBuffers');
    cbStoreHistoryDup.Checked:= Reg.ValueExists('HistoryNoDup');
    if cbStoreHistoryDup.Checked then cbNoDuplicates.Checked:= Reg.ReadBool('HistoryNoDup');
    cbStoreWinSize.Checked:= Reg.ValueExists('WindowSize');
    if cbStoreWinSize.Checked then begin
      ws:= Reg.ReadInteger('WindowSize');
      seWinSizeX.Value:= hi(ws);
      seWinSizeY.Value:= lo(ws);
    end;
    cbStoreWinPos.Checked:= Reg.ValueExists('WindowPosition');
    if cbStoreWinPos.Checked then begin
      ws:= Reg.ReadInteger('WindowPosition');
      seWinPosX.Value:= hi(ws);
      seWinPosY.Value:= lo(ws);
    end;
    cbStoreScrBufSize.Checked:= Reg.ValueExists('ScreenBufferSize');
    if cbStoreScrBufSize.Checked then begin
      ws:= Reg.ReadInteger('ScreenBufferSize');
      seScrBufSizeX.Value:= hi(ws);
      seScrBufSizeY.Value:= lo(ws);
    end;
    cbStoreFont.Checked:= Reg.ValueExists('FontFamily') and (Reg.ReadInteger('FontFamily') in [$30, $36]);
    if cbStoreFont.Checked then begin
      ws:= Reg.ReadInteger('FontFamily');
      case ws of
        $30: begin
          cbFontFace.ItemIndex:= 0;
          ws:= Reg.ReadInteger('FontSize');
          cbFontSizeFix.Text:= format('%d x %d', [hi(ws), lo(ws)]);
        end;
        $36: begin
          if Reg.ValueExists('FaceName') then
            cbFontFace.ItemIndex:= cbFontFace.Items.IndexOf(Reg.ReadString('FaceName'))
          else
            cbFontFace.ItemIndex:= 1;
          ws:= Reg.ReadInteger('FontSize');
          cbFontSizeVec.Text:= IntToStr(hi(ws));
        end;
      end;
      cbFontFaceSelect(Self);
      cbFontBold.Checked:= Reg.ReadInteger('FontWeight') > 0;
    end;
    EnableControls;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmConsole.cbFontFaceSelect(Sender: TObject);
begin
  cbFontSizeVec.Visible:= cbFontFace.ItemIndex > 0;
  cbFontBold.Visible:= cbFontFace.ItemIndex > 0;
  cbFontSizeFix.Visible:= cbFontFace.ItemIndex = 0;
end;

procedure TfrmConsole.btnSaveChangesClick(Sender: TObject);
var
  Reg: TRegistry;
  ws,a,b: DWord;
begin
  Reg:= TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    case cbWindowNameSel.ItemIndex of
      0: Reg.OpenKey(sConsoleRegKey, false);
    else
      Reg.OpenKey(sConsoleRegKey + '\' +  cbWindowNameSel.Text, false);
    end;

    if cbStoreCursorSize.Checked then
      Reg.WriteInteger('CursorSize', StrToInt(cbCursorSize.Text))
    else
      Reg.DeleteValue('CursorSize');

    if cbStoreQuickedit.Checked then
      Reg.WriteBool('QuickEdit', cbQuickEdit.Checked)
    else
      Reg.DeleteValue('QuickEdit');

    if cbStoreInsertMode.Checked then
      Reg.WriteBool('InsertMode', cbInsertMode.Checked)
    else
      Reg.DeleteValue('InsertMode');

    if cbStoreHistoryLen.Checked then
      Reg.WriteInteger('HistoryBufferSize', seHistoryLength.Value)
    else
      Reg.DeleteValue('HistoryBufferSize');

    if cbStoreHistoryNum.Checked then
      Reg.WriteInteger('NumberOfHistoryBuffers', seHistoryBufferCount.Value)
    else
      Reg.DeleteValue('NumberOfHistoryBuffers');

    if cbStoreHistoryDup.Checked then
      Reg.WriteBool('HistoryNoDup', cbNoDuplicates.Checked)
    else
      Reg.DeleteValue('HistoryNoDup');

    if cbStoreWinSize.Checked then
      Reg.WriteInteger('WindowSize', MakeLongWord(seWinSizeX.Value, seWinSizeY.Value))
    else
      Reg.DeleteValue('WindowSize');

    if cbStoreWinPos.Checked then
      Reg.WriteInteger('WindowPosition', MakeLongWord(seWinPosX.Value, seWinPosY.Value))
    else
      Reg.DeleteValue('WindowPosition');

    if cbStoreScrBufSize.Checked then
      Reg.WriteInteger('ScreenBufferSize', MakeLongWord(seScrBufSizeX.Value, seScrBufSizeY.Value))
    else
      Reg.DeleteValue('ScreenBufferSize');

    if cbStoreFont.Checked then begin
      case cbFontFace.ItemIndex of
        0: begin
          Reg.WriteInteger('FontFamily', $30);
          SScanf(cbFontSizeFix.Text, '%d x %d', [@a,@b]);
          ws:= MakeLongWord(b,a);
          Reg.WriteInteger('FontSize', ws);
        end;
        else begin
          Reg.WriteInteger('FontFamily', $36);
          Reg.WriteString('FaceName', cbFontFace.Text);
          ws:= MakeLongWord(StrToInt(cbFontSizeVec.Text), 0);
          Reg.WriteInteger('FontSize', ws);
        end;
      end;
      if cbFontBold.Checked then
        Reg.WriteInteger('FontWeight', 700)
      else
        Reg.WriteInteger('FontWeight', 0);
    end else begin
      Reg.DeleteValue('FontFamily');
      Reg.DeleteValue('FontSize');
      Reg.DeleteValue('FontWeight');
      Reg.DeleteValue('FaceName');
    end;
    EnableControls;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmConsole.btnDeleteWindowKeyClick(Sender: TObject);
var
  Reg: TRegistry;
begin
  Reg:= TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    case cbWindowNameSel.ItemIndex of
      0: exit;
    else
      Reg.OpenKey(sConsoleRegKey, false);
    end;
    Reg.DeleteKey(cbWindowNameSel.Text);
  finally
    FreeAndNil(Reg);
  end;
  ReadItems;
end;

procedure TfrmConsole.cbStoreChecked(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmConsole.EnableControls;
begin
  cbCursorSize.Enabled:= cbStoreCursorSize.Checked;
  cbQuickEdit.Enabled:= cbStoreQuickedit.Checked;
  cbInsertMode.Enabled:= cbStoreInsertMode.Checked;
  seHistoryLength.Enabled:= cbStoreHistoryLen.Checked;
  seHistoryBufferCount.Enabled:= cbStoreHistoryNum.Checked;
  cbNoDuplicates.Enabled:= cbStoreHistoryDup.Checked;
  seWinSizeX.Enabled:= cbStoreWinSize.Checked;
  seWinSizeY.Enabled:= cbStoreWinSize.Checked;
  seScrBufSizeX.Enabled:= cbStoreScrBufSize.Checked;
  seScrBufSizeY.Enabled:= cbStoreScrBufSize.Checked;
  seWinPosX.Enabled:= cbStoreWinPos.Checked;
  seWinPosY.Enabled:= cbStoreWinPos.Checked;
  cbAutomatic.Enabled:= cbStoreWinposAuto.Checked;
  cbFontFace.Enabled:= cbStoreFont.Checked;
  cbFontBold.Enabled:= cbStoreFont.Checked;
  cbFontSizeFix.Enabled:= cbStoreFont.Checked;
  cbFontSizeVec.Enabled:= cbStoreFont.Checked;
end;



end.

