unit uCSysParametersInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

type
  ESPIException = class (EOSError);
  TSystemParametersInfo = class
  private
  protected
    procedure SPIChecked(const action: UINT; const LData: UInt; const PData: Pointer; const Update: boolean);
  public
    procedure GetBlob(const Action: UINT; LData: UINT; PData: Pointer);
    procedure SetBlob(const Action: UINT; LData: UINT; PData: Pointer);
    function GetBool(const Action: UINT): Boolean;
    procedure SetBool(const Action: UINT; const Value: Boolean);
    procedure SetBoolParam(const Action: UINT; const Value: Boolean);
    function GetUInt(const Action: UINT): UINT;
    procedure SetUInt(const Action: UINT; const Value: UINT);
  end;

implementation

{ TSystemParametersInfo }

procedure TSystemParametersInfo.SPIChecked(const action: UINT; const LData: UInt; const PData: Pointer; const Update: boolean);
var
  f: DWORD;
begin
  if Update then
    f:= SPIF_UPDATEINIFILE or SPIF_SENDWININICHANGE
  else
    f:= 0;
  if not SystemParametersInfo(action, LData, PData, f) then
    raise ESPIException.CreateFmt('SPI call failed, last error = %d', [GetLastError]);
end;

procedure TSystemParametersInfo.GetBlob(const Action: UINT; LData: UINT; PData: Pointer);
begin
  SPIChecked(Action, LData, PData, false);
end;

procedure TSystemParametersInfo.SetBlob(const Action: UINT; LData: UINT; PData: Pointer);
begin
  SPIChecked(Action, LData, PData, true);
end;

function TSystemParametersInfo.GetBool(const Action: UINT): Boolean;
var
  x: BOOL;
begin
  SPIChecked(Action, 0, @x, false);
  Result:= X;
end;

procedure TSystemParametersInfo.SetBool(const Action: UINT; const Value: Boolean);
var
  x: BOOL;
begin
  X:= Value;
  SPIChecked(Action, 0, @x, True);
end;

procedure TSystemParametersInfo.SetBoolParam(const Action: UINT; const Value: Boolean);
begin
  SPIChecked(Action, LONG(LongBool(Value)), nil, True);
end;

function TSystemParametersInfo.GetUInt(const Action: UINT): UINT;
begin
  SPIChecked(Action, 0, @Result, False);
end;

procedure TSystemParametersInfo.SetUInt(const Action: UINT; const Value: UINT);
begin
  SPIChecked(Action, 0, Pointer(Value), True);
end;

end.

