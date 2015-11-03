library SimbaExt;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
{$define experiments}

uses
  SysUtils,
  Classes,
  Math,
  dynlibs,
  DeclManager,
  
  Arrays,
  CoreMath,
  CoreMisc,
  CoreTypes,
  CornerDet,
  DensityMap,
  Dictionary,
  ExceptionMgr,
  Finder,
  Imaging,
  MatrixOps,
  Morphology,
  PointTools,
  Randomize,
  Std,
  Sorting,
  Statistics,
  Spline,
  StringTools,
  TimeUtils,
  Trees{$IFDEF experiments},{$ELSE};{$ENDIF}
  {$IFDEF experiments}tests;{$ENDIF}

var
  Methods: array of record ProcAddr: Pointer; ProcDef:AnsiString; end;
  MethodsLoaded: Boolean = False;
  TypeDefs: array of record TypeName, TypeDef:PChar; end;
  TypesLoaded: Boolean = False;

  ClassManager: TManagedDeclarations;
  
var
  OldMemoryManager: TMemoryManager;
  MemIsset: Boolean = False;


type
  SExt = NativeInt;
  
  
//used for debugging exports
function MethodList(var se:SExt): TStringArray; cdecl;
var i:Int32;
begin
  SetLength(Result, Length(methods));
  for i:=0 to High(Methods) do Result[i] := Methods[i].ProcDef;
end;

procedure FreeDeclarations(var se:SExt); cdecl;
begin
  ClassManager.ReleaseThread(se);
end;


//Include export wrappers
{$I lpexports/wrap_arrays.inc}
{$I lpexports/wrap_std.inc}
{$I lpexports/wrap_math.inc}
{$I lpexports/wrap_finder.inc}
{$I lpexports/wrap_sorting.inc}
{$I lpexports/wrap_imaging.inc}
{$I lpexports/wrap_matrixops.inc}
{$I lpexports/wrap_statistics.inc}
{$I lpexports/wrap_pointtools.inc}
{$I lpexports/wrap_stringtools.inc}
{$I lpexports/wrappers_general.inc}


procedure AddMethod(ProcAddr: Pointer; ProcDef: AnsiString);
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
var
  _TYPENAME,_T,_TOvf:String;
begin
  MethodsLoaded := True;
  {$I export_methods.inc}
end;


procedure LoadTypes;
begin
  TypesLoaded := True;
  {$I export_types.inc}
end;


function GetPluginABIVersion: Integer; cdecl; export;
begin
  Result := 2;
end;


procedure SetPluginMemManager(MemMgr : TMemoryManager); cdecl; export;
begin
  if MemIsSet then Exit;
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(MemMgr);
  MemIsSet := True;
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
  end;
end;



exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetTypeCount;
exports GetTypeInfo;
exports GetFunctionCount;
exports GetFunctionInfo;
exports OnDetach;



initialization
  ClassManager := TManagedDeclarations.Create();
end.
