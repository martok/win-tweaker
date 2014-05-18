unit uFrmSysParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, windows, Dialogs,
  Buttons, Spin, ExtCtrls, ComCtrls, registry, messages;

type
  { WARNING: DO NOT USE}
  TNONCLIENTMETRICS0600 = packed record
    NCM: NONCLIENTMETRICS;
    { SystemParametersInfo(SPI_GETNONCLIENTMETRICS) is broken on 8.1 and returns iPaddedBorderWidth=0.
      What's even worse: the 0 gets written to the registry correctly - Get followed by Set is not a NOOP!
    }
    iPaddedBorderWidth : longint;
  end;

  TfrmSysParameters = class(TFrame)
    btnSaveChanges: TBitBtn;
    btnReload: TBitBtn;
    cbFontSmoothing: TCheckBox;
    cbFontSmoothingOrientation: TComboBox;
    cbFontSmoothingType: TComboBox;
    cbIconWrapCaption: TCheckBox;
    cbMouseWheel: TCheckBox;
    FontDialog1: TFontDialog;
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
    pcSPI: TPageControl;
    pnFontCaption: TPanel;
    pnFontIcons: TPanel;
    pnFontMenu: TPanel;
    pnFontMessage: TPanel;
    pnFontSmCaption: TPanel;
    pnFontStatus: TPanel;
    seBorderWidth: TSpinEdit;
    seCaptionHeight: TSpinEdit;
    seCaptionWidth: TSpinEdit;
    seMenuHeight: TSpinEdit;
    seMenuWidth: TSpinEdit;
    seScrollBarHeight: TSpinEdit;
    seIconSpacingY: TSpinEdit;
    seScrollBarWidth: TSpinEdit;
    seIconSpacingX: TSpinEdit;
    seSmCaptionHeight: TSpinEdit;
    seSmCaptionWidth: TSpinEdit;
    seFontSmoothingContrast: TSpinEdit;
    stClearType: TTabSheet;
    tsMouse: TTabSheet;
    tsIcons: TTabSheet;
    tsNonClientArea: TTabSheet;
    procedure btnReloadClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure FontClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure AfterConstruction; override;
  end;

implementation

uses
  uCSysParametersInfo;

{$R *.lfm}

const
  sCoverWindowSwitch = 'HKCU\Software\Microsoft\Windows\CurrentVersion\Themes\NoCoverWindow';
  sWindowMetricsRegKey = '\Control Panel\Desktop\WindowMetrics';

const
  SPI_GETNONCLIENTMETRICS   = $0029;
  SPI_GETMOUSEWHEELROUTING  = $201C;
  SPI_SETMOUSEWHEELROUTING  = $201D;
  MOUSEWHEEL_ROUTING_FOCUS  = -9999; // Constant unknown...
  MOUSEWHEEL_ROUTING_HYBRID = -9999; // Constant unknown...

procedure CBAddIntConsts(const CB: TComboBox; const Values: array of UInt; const Names: array of String; const Clear: boolean = false);
var
  i: integer;
begin
  if Clear then
    CB.Clear;
  for i:= 0 to high(Values) do
    CB.AddItem(Names[i], TObject(Pointer(Values[i])));
end;

function CBIntConstValue(const CB: TComboBox): UInt;
begin
  Result:= PtrUInt(CB.Items.Objects[CB.ItemIndex]);
end;

{ TfrmSysParameters }

procedure TfrmSysParameters.btnReloadClick(Sender: TObject);
var
  NCM: TNONCLIENTMETRICS;
  icm: TICONMETRICS;
  spi: TSystemParametersInfo;
begin
  spi:= TSystemParametersInfo.Create;
  try
    // Non-Client
    ncm.cbSize:= SizeOf(NCM);
    spi.GetBlob(SPI_GETNONCLIENTMETRICS, ncm.cbSize, @ncm);

    seBorderWidth.Value:= NCM.iBorderWidth;

    seScrollBarWidth.Value:= NCM.iScrollWidth;
    seScrollBarHeight.Value:= NCM.iScrollHeight;

    seCaptionWidth.Value:= NCM.iCaptionWidth;
    seCaptionHeight.Value:= NCM.iCaptionHeight;

    seSmCaptionWidth.Value:= NCM.iSmCaptionWidth;
    seSmCaptionHeight.Value:= NCM.iSmCaptionHeight;

    seMenuWidth.Value:= NCM.iMenuWidth;
    seMenuHeight.Value:= NCM.iMenuHeight;

    pnFontCaption.Font.Handle:= CreateFontIndirect(NCM.lfCaptionFont);
    pnFontCaption.Color:= GetSysColor(COLOR_ACTIVECAPTION);
    pnFontCaption.Font.Color:= GetSysColor(COLOR_CAPTIONTEXT);

    pnFontSmCaption.Font.Handle:= CreateFontIndirect(NCM.lfSmCaptionFont);
    pnFontSmCaption.Color:= GetSysColor(COLOR_ACTIVECAPTION);
    pnFontSmCaption.Font.Color:= GetSysColor(COLOR_CAPTIONTEXT);

    pnFontMenu.Font.Handle:= CreateFontIndirect(NCM.lfMenuFont);
    pnFontMenu.Color:= GetSysColor(COLOR_MENU);
    pnFontMenu.Font.Color:= GetSysColor(COLOR_MENUTEXT);

    pnFontStatus.Font.Handle:= CreateFontIndirect(NCM.lfStatusFont);

    pnFontMessage.Font.Handle:= CreateFontIndirect(NCM.lfMessageFont);

    // ClearType
    cbFontSmoothing.Checked:= spi.GetBool(SPI_GETFONTSMOOTHING);
    seFontSmoothingContrast.Value:= spi.GetUInt(SPI_GETFONTSMOOTHINGCONTRAST);
    cbFontSmoothingOrientation.ItemIndex:= cbFontSmoothingOrientation.Items.IndexOfObject(TObject(Pointer(spi.GetUInt(SPI_GETFONTSMOOTHINGORIENTATION))));
    cbFontSmoothingType.ItemIndex:= cbFontSmoothingType.Items.IndexOfObject(TObject(Pointer(spi.GetUInt(SPI_GETFONTSMOOTHINGTYPE))));

    // Icons
    icm.cbSize:= sizeof(icm);
    spi.GetBlob(SPI_GETICONMETRICS, icm.cbSize, @icm);
    cbIconWrapCaption.Checked:= icm.iTitleWrap <> 0;
    seIconSpacingX.Value:= 0;
    seIconSpacingX.Value:= icm.iHorzSpacing;
    seIconSpacingX.MinValue:= GetSystemMetrics(SM_CXICON);
    seIconSpacingY.Value:= 0;
    seIconSpacingY.Value:= icm.iVertSpacing;
    seIconSpacingY.MinValue:= GetSystemMetrics(SM_CYICON);
    pnFontIcons.Font.Handle:= CreateFontIndirect(icm.lfFont);

    // Mouse
    cbMouseWheel.Checked:= spi.GetUInt(SPI_GETMOUSEWHEELROUTING) in [MOUSEWHEEL_ROUTING_FOCUS, 0];

  finally
    FreeAndNil(spi);
  end;
end;

procedure TfrmSysParameters.btnSaveChangesClick(Sender: TObject);
var
  NCM: TNONCLIENTMETRICS;
  icm: TICONMETRICS;
  spi: TSystemParametersInfo;
begin
  Screen.Cursor:= crHourGlass;
  spi:= TSystemParametersInfo.Create;
  try
    // Non-Client
    ncm.cbSize:= SizeOf(NCM);
    spi.GetBlob(SPI_GETNONCLIENTMETRICS, ncm.cbSize, @ncm);

    NCM.iBorderWidth:= seBorderWidth.Value;

    NCM.iScrollWidth:= seScrollBarWidth.Value;
    NCM.iScrollHeight:= seScrollBarHeight.Value;

    NCM.iCaptionWidth:= seCaptionWidth.Value;
    NCM.iCaptionHeight:= seCaptionHeight.Value;

    NCM.iSmCaptionWidth:= seSmCaptionWidth.Value;
    NCM.iSmCaptionHeight:= seSmCaptionHeight.Value;

    NCM.iMenuWidth:= seMenuWidth.Value;
    NCM.iMenuHeight:= seMenuHeight.Value;

    GetObject(pnFontCaption.Font.Handle, sizeof(NCM.lfCaptionFont), @NCM.lfCaptionFont);
    GetObject(pnFontSmCaption.Font.Handle, sizeof(NCM.lfSmCaptionFont), @NCM.lfSmCaptionFont);
    GetObject(pnFontMenu.Font.Handle, sizeof(NCM.lfMenuFont), @NCM.lfMenuFont);
    GetObject(pnFontStatus.Font.Handle, sizeof(NCM.lfStatusFont), @NCM.lfStatusFont);
    GetObject(pnFontMessage.Font.Handle, sizeof(NCM.lfMessageFont), @NCM.lfMessageFont);

    spi.SetBlob(SPI_SETNONCLIENTMETRICS, ncm.cbSize, @ncm);

    // ClearType
    spi.SetBoolParam(SPI_SETFONTSMOOTHING, cbFontSmoothing.Checked);
    spi.SetUInt(SPI_SETFONTSMOOTHINGCONTRAST, seFontSmoothingContrast.Value);
    spi.SetUInt(SPI_SETFONTSMOOTHINGTYPE, CBIntConstValue(cbFontSmoothingType));
    spi.SetUInt(SPI_SETFONTSMOOTHINGORIENTATION, CBIntConstValue(cbFontSmoothingOrientation));

    // Icons
    icm.cbSize:= sizeof(icm);
    spi.GetBlob(SPI_GETICONMETRICS, icm.cbSize, @icm);
    icm.iTitleWrap:= Ord(cbIconWrapCaption.Checked);
    icm.iHorzSpacing:= seIconSpacingX.Value;
    icm.iVertSpacing:= seIconSpacingY.Value;
    GetObject(pnFontIcons.Font.Handle, sizeof(icm.lfFont), @icm.lfFont);
    spi.SetBlob(SPI_SETICONMETRICS, icm.cbSize, @icm);
  finally
    Screen.Cursor:= crDefault;
    FreeAndNil(spi);
  end;
  btnReload.Click;
end;

procedure TfrmSysParameters.FontClick(Sender: TObject);
begin
  FontDialog1.Font:= (Sender as TPanel).Font;
  if FontDialog1.Execute then
    (Sender as TPanel).Font:= FontDialog1.Font;
end;

procedure TfrmSysParameters.AfterConstruction;
begin
  inherited AfterConstruction;
  CBAddIntConsts(cbFontSmoothingOrientation, [FE_FONTSMOOTHINGORIENTATIONBGR, FE_FONTSMOOTHINGORIENTATIONRGB],
                                             ['FE_FONTSMOOTHINGORIENTATIONBGR', 'FE_FONTSMOOTHINGORIENTATIONRGB'], true);
  CBAddIntConsts(cbFontSmoothingType, [FE_FONTSMOOTHINGSTANDARD, FE_FONTSMOOTHINGCLEARTYPE],
                                      ['FE_FONTSMOOTHINGSTANDARD', 'FE_FONTSMOOTHINGCLEARTYPE'], true);
  btnReload.Click;
end;

end.


