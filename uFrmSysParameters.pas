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
    FontDialog1: TFontDialog;
    Label1: TLabel;
    Label10: TLabel;
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
    seScrollBarWidth: TSpinEdit;
    seSmCaptionHeight: TSpinEdit;
    seSmCaptionWidth: TSpinEdit;
    seFontSmoothingContrast: TSpinEdit;
    stClearType: TTabSheet;
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
  SPI_GETNONCLIENTMETRICS = $0029;
  CM_WININICHANGE = $b025;
  CM_SYSFONTCHANGED = $b035;

{ TfrmSysParameters }

procedure TfrmSysParameters.btnReloadClick(Sender: TObject);
var
  NCM: TNONCLIENTMETRICS;
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
  finally
    FreeAndNil(spi);
  end;
end;

procedure TfrmSysParameters.btnSaveChangesClick(Sender: TObject);
var
  NCM: TNONCLIENTMETRICS;
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
    spi.SetUInt(SPI_SETFONTSMOOTHINGTYPE, PtrUInt(cbFontSmoothingType.Items.Objects[cbFontSmoothingType.ItemIndex]));
    spi.SetUInt(SPI_SETFONTSMOOTHINGORIENTATION, PtrUInt(cbFontSmoothingOrientation.Items.Objects[cbFontSmoothingOrientation.ItemIndex]));
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
  cbFontSmoothingOrientation.Clear;
  cbFontSmoothingOrientation.AddItem('FE_FONTSMOOTHINGORIENTATIONBGR', TObject(Pointer(FE_FONTSMOOTHINGORIENTATIONBGR)));
  cbFontSmoothingOrientation.AddItem('FE_FONTSMOOTHINGORIENTATIONRGB', TObject(Pointer(FE_FONTSMOOTHINGORIENTATIONRGB)));
  cbFontSmoothingType.Clear;
  cbFontSmoothingType.AddItem('FE_FONTSMOOTHINGCLEARTYPE', TObject(Pointer(FE_FONTSMOOTHINGCLEARTYPE)));
  cbFontSmoothingType.AddItem('FE_FONTSMOOTHINGSTANDARD', TObject(Pointer(FE_FONTSMOOTHINGSTANDARD)));
  btnReload.Click;
end;

end.


