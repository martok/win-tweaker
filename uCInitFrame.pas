unit uCInitFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TFrameClass = class of TFrame;
  IInitializable = interface
    ['{574DBB62-877B-4A3D-B4BC-71E63484BB7B}']
    procedure Initialize;
  end;

implementation

end.

