library SimbaExt;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{$mode objfpc}{$H+}
{$macro on}
{$inline on}

uses
  SysUtils,
  Classes,
  Math,

  CoreTypes,
  ExceptionMgr,
  CoreMath,
  CoreMisc,
  MatrixMath,
  MatrixOps,
  Std,
  PointList,
  Sorting,
  MatrixTools,
  ColorMath,
  HashMap,
  Numeric,
  Imaging,
  Randomize,
  PointTools,
  Finder,
  SimpleOCR,
  Spline,
  Morphology,
  DensityMap,
  StringTools,
  CornerDet;
  

//Include export wrappers
{$I ExportWrap.inc}

var
  Methods: array of record procAddr: Pointer; ProcDef:PChar; end;
  MethodsLoaded: Boolean = False;
  TypeDefs: array of record TypeName, TypeDef:PChar; end;
  TypesLoaded: Boolean = False;
  
  OldMemoryManager: TMemoryManager;
  MemIsset: Boolean = False;


procedure AddMethod(ProcAddr: Pointer; ProcDef: PChar);
var L: Integer;
begin
  L := Length(Methods);
  SetLength(Methods, L + 1);
  Methods[l].ProcAddr := ProcAddr;
  Methods[l].ProcDef := ProcDef;
end;


procedure AddType(TypeName, TypeDef: PChar);
var L: Integer;
begin
  L := Length(TypeDefs);
  SetLength(TypeDefs, L + 1);
  TypeDefs[l].TypeName := TypeName;
  TypeDefs[l].TypeDef := TypeDef;
end;


procedure LoadMethods;
begin
  {$I exports.inc}
  MethodsLoaded := True;
end;

procedure FreeMethods;
begin
  SetLength(Methods, 0);
  MethodsLoaded := False;
end;


procedure LoadTypes;
begin
  {$I export_types.inc}
  TypesLoaded := True;
end;

procedure FreeTypes;
begin
  SetLength(TypeDefs, 0);
  TypesLoaded := False;
end;


function GetPluginABIVersion: Integer; cdecl; export;
begin
  Result := 2;
end;

procedure SetPluginMemManager(MemMgr : TMemoryManager); cdecl; export;
begin
  if memisset then
    exit;
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(MemMgr);
  memisset := true;
end;



procedure OnDetach; cdecl; export;
begin
  SetMemoryManager(OldMemoryManager);
end;



function GetFunctionCount: Integer; cdecl; export;
begin
  if not MethodsLoaded then
    LoadMethods;
  Result := Length(Methods);
end;

function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; cdecl; export;
begin
  Result := x;
  if (x > -1) and InRange(x, 0, High(Methods)) then
  begin
    ProcAddr := Methods[x].procAddr;
    StrPCopy(ProcDef, Methods[x].ProcDef);
    if (x = High(Methods)) then FreeMethods;
  end;
end;



function GetTypeCount: Integer; cdecl; export;
begin
  if not TypesLoaded then
    LoadTypes;
  Result := Length(TypeDefs);
end;

function GetTypeInfo(x: Integer; var TypeName, TypeDef: PChar): integer; cdecl; export;
begin
  Result := x;
  if (x > -1) and InRange(x, 0, High(TypeDefs)) then
  begin
    StrPCopy(TypeName, TypeDefs[x].TypeName);
    StrPCopy(TypeDef,  TypeDefs[x].TypeDef);
    
    if (x = High(TypeDefs)) then FreeTypes;
  end;
end;



exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetTypeCount;
exports GetTypeInfo;
exports GetFunctionCount;
exports GetFunctionInfo;
exports OnDetach;


end.
