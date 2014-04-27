library SEExtra;
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
  Windows,
  Math,

  TimeUtils,
  OsUtils;

//Include Simba Wrapper
//{$I ExportWrap.pas}

type
  TCommand = record
    procAddr: Pointer;
    procDef: PChar;
  end;

var
  commands: array of TCommand;
  commandsLoaded: Boolean;
  OldMemoryManager: TMemoryManager;
  memisset: Boolean = False;


procedure AddCommand(ProcAddr: Pointer; ProcDef: PChar);
var L: Integer;
begin
  L := Length(commands);
  SetLength(commands, L + 1);
  commands[L].ProcAddr := ProcAddr;
  commands[L].ProcDef := ProcDef;
end;


{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=  THIS GOES OUT OF OUR PLUGIN  =-=-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure SetupCommands;
begin
  // TimeUtils.pas
  AddCommand(@TimeSinceEpoch,	'function exp_TimeSinceEpoch(OffsetSec:UInt32=0): Double;');
  AddCommand(@FormatEpochTime,	'function exp_FormatEpochTime(FormatString:String; UnixTime: Double): String;');
  AddCommand(@EpochToTime,	'function exp_EpochToTime(UnixTime: Double): TDateTime;');

  // OSUtils.pas (OS.*
  AddCommand(@GetEnviron,     'function exp_GetEnviron(Varname:String): String;');
  AddCommand(@SetEnviron,     'function exp_SetEnviron(Varname, Value:String): Boolean;');
  AddCommand(@ListDir,        'procedure exp_ListDir(Path:String; out contents:TStringArray);');
  AddCommand(@FileRemove,     'function exp_FileRemove(Path:String): Boolean;');
  AddCommand(@DirRemove,      'function exp_DirRemove(Path:String): Boolean;');
  AddCommand(@FileRename,     'function exp_FileRename(Src, Dest:String): Boolean;');
  AddCommand(@FileCopy,       'function exp_CopyFile(Src, Dest:String): Boolean;');
  AddCommand(@FileSize2,      'function exp_FileSize2(Path: String): Int64;');
  AddCommand(@ReadFile,       'function exp_ReadFile(FileName: String): String;');
  AddCommand(@UTime,          'function exp_UTime(Path:String; Times:TIntArray): Boolean;');
  AddCommand(@TouchFile,      'procedure exp_TouchFile(Path:String);');

  // OSUtils.pas (OS.Path.*)
  AddCommand(@NormPath,     'function exp_NormPath(Path:String): String;');
  AddCommand(@AbsPath,      'function exp_AbsPath(Path:String): String;');
  AddCommand(@DirName,      'function exp_DirName(Path:String): String;');
  AddCommand(@PathExists,   'function exp_PathExists(Path:String): Boolean;');
  AddCommand(@IsAbsPath,    'function exp_IsAbsPath(path:String): Boolean;');
  AddCommand(@IsFile,       'function exp_IsFile(path:String): Boolean;');
  AddCommand(@IsDir,        'function exp_IsDir(path:String): Boolean;');
end;




procedure UnsetupCommands;
begin
  SetLength(commands, 0);
  CommandsLoaded := False;
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
  if not commandsLoaded then
    SetupCommands;
  Result := Length(commands);
end;


function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; cdecl; export;
begin
  if ((x > -1) and InRange(x, Low(commands), High(commands))) then
  begin
    ProcAddr := commands[x].procAddr;
    StrPCopy(ProcDef, commands[x].procDef);
    if (x = High(commands)) then UnsetupCommands;
    Exit(x);
  end;
  Exit(-1);
end;


exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetFunctionCount;
exports GetFunctionInfo;
exports OnDetach;

end.
