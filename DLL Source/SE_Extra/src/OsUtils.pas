unit OSUtils;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
interface

uses Windows,SysUtils,CoreTypes;

function GetEnviron(const Varname:String): String; Cdecl;
function SetEnviron(const Varname, Value:String): Boolean; Cdecl;
procedure ListDir(Path:String; out contents:TStringArray); Cdecl;
function FileRemove(Path:String): Boolean; Cdecl;
function DirRemove(Path:String): Boolean; Cdecl;
function FileRename(Src, Dest:String): Boolean; Cdecl;
function FileCopy(Src, Dest:String): Boolean; Cdecl;
function FileSize2(Path: String): Int64; Cdecl;
function ReadFile(FileName: String): String; Cdecl;
function UTime(Path:String; Times:TIntArray): Boolean; Cdecl;
procedure TouchFile(Path:String); Cdecl;

//os.path
function NormPath(Path:String): String; Cdecl;
function AbsPath(Path:String): String; cdecl;
function DirName(Path:String): String; Cdecl;
function PathExists(Path:String): Boolean; Cdecl;
function IsAbsPath(path:String): Boolean; Cdecl;
function IsFile(path:String): Boolean; Cdecl;
function IsDir(path:String): Boolean; Cdecl;

//----------------------------------------------------------------------------\\
implementation

uses
  Classes, FileUtil, DateUtils, TimeUtils, StringTools;

function GetCWD: string;
var
  tmp:TStringArray;
  i: Int32;
begin
  (* SimbaPath = ModuleDirectory - \incudes\WhatEver\plugin.dll *)
  Result := LowerCase(GetModuleName(hInstance));
  tmp := StrExplode(Result, '\');
  Result := '';
  for i:=0 to (High(tmp) - 3) do
    Result := Result + tmp[i] + '\';
end;

function FixDirPath(path:String): String;
begin
  if DirectoryExists(Path) and (Path[length(path)] <> '\') then
    Path := Path + '\';
  Result := Path;
end;


(* OS.Path.* and whatnots *******************************************************)

// os.path.normpath(path)
function NormPath(Path:String): String; cdecl;
var
  i,j,slash:Int32;
  fold:TStringArray;
  tmp:String;
  NotBack: Boolean;
begin
  Path := StringReplace(Path, '/', '\', [rfReplaceAll]);
  tmp := '';
  slash := 0;
  for i:=1 to Length(path) do
  begin
    if path[i] = '\' then inc(slash)
    else slash := 0;
    if not(slash > 1) then tmp := tmp + path[i];
  end;
  fold := StrExplode(tmp,'\');
  j := 0;
  Result := '';
  for i:=0 to High(fold) do
  begin
    notback := True;
    if (i < High(fold)) then
      notback := (fold[i+1] <> '..');
    if notback and (fold[i] <> '..') and (fold[i] <> '.') then
    begin
      if j > 0 then
        Result := Result + '\' + fold[i]
      else
        Result := Result + Fold[i];
      inc(j);
    end;
  end;
end;


// os.path.abspath(path)
function AbsPath(Path:String): String; cdecl;
begin
  if length(path) > 2 then
  begin
    if not((lowercase(path[1]) in ['a'..'z']) and (path[2] = ':')) then
      Result := NormPath(GetCWD() + Path);
  end else
    Result := NormPath(GetCWD() + Path);
end;

// os.path.basename(path)
// os.path.dirname(path)
function DirName(Path:String): String; Cdecl;
var
  a:TStrArray;
  i:Int32;
begin
  Path := FixDirPath(NormPath(Path));
  a := StrExplode(Path,'\');
  if Length(a) > 0 then
  begin
    Result := a[0];
    for i:=1 to High(a)-1 do
      Result := Result + '\' + a[i];
  end;
end;


// os.path.exists(path)
function PathExists(Path:String): Boolean; Cdecl;
begin
  Result := IsFile(Path) or IsDir(Path);
end;


// os.path.getatime(path)
// os.path.getmtime(path)
// os.path.getsize(path)
// os.path.isabs(path)
function IsAbsPath(path:String): Boolean; Cdecl;
begin
  if Length(path) > 2 then
    Result := (lowercase(path[1]) in ['a'..'z']) and (path[2] = ':')
  else
    Result := False;
  //if not(Result) and ((path[1] = '\') or (path[1] = '/')) then
  //  Result := True;
end;

// os.path.isfile(path)
function IsFile(path:String): Boolean; Cdecl;
begin
  Result := FileExists(Path);
end;

// os.path.isdir(path)
function IsDir(path:String): Boolean; Cdecl;
begin
  Result := DirectoryExists(Path);
end;

// os.path.search(path, file)




(* OS.* and whatnots **********************************************************)



{ OS.GetEnv(Varname:String): String; }
function GetEnviron(const Varname:String): String; Cdecl;
begin
  Result := GetEnvironmentVariableUTF8(Varname);
end;

{ OS.SetEnv(Varname, Value:String): Boolean; }
function SetEnviron(const Varname, Value:String): Boolean; Cdecl;
begin
  Result := SetEnvironmentVariableW(PWChar(Varname), PWChar(Value));
end;

{ OS.ListDir(Path:String): TStringArray }
procedure ListDir(Path:String; out contents:TStringArray); Cdecl;
var
  l: Int32;
  SR : TSearchRec;
begin
  Path := FixDirPath(NormPath(Path));
  l := 0;
  if FindFirst(Path + '*', faAnyFile and faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        inc(l);
        SetLength(contents, l);
        contents[l-1] := SR.Name;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;


{ OS.Remove(Path:String): Boolean; }
function FileRemove(Path:String): Boolean; Cdecl;
begin
  Result := DeleteFileUTF8(Path);
end;


{ OS.Rmdir(Path:String): Boolean; }
function DirRemove(Path:String): Boolean; Cdecl;
var
  Files:TStringArray;
  i:Int32;
begin
  Path := FixDirPath(NormPath(Path));
  ListDir(Path, Files);
  for i:=0 to High(Files) do
  begin
    if not IsDir(Path + Files[i]) then
      DeleteFileUTF8(Path + Files[i])
    else 
      DirRemove(Path + Files[i]);
  end;
  Result := RemoveDirUTF8(Path);
end;


{ OS.Rename(Src, Dest:String) }
function FileRename(Src, Dest:String): Boolean; Cdecl;
begin
  Result := RenameFileUTF8(Src, Dest);
end;


{ OS.CopyFile(Src, Dest:String) }
function FileCopy(Src, Dest:String): Boolean; Cdecl;
begin
  Result := CopyFile(Src, Dest);
end;


{ OS.Size(Path:String) }
function FileSize2(Path: String): Int64; Cdecl;
var
  i:Int32;
  list: TStringList;
  size: Int64;
begin
  Size := 0;
  if IsFile(path) then
    Size := FileSize(Path)
  else begin
    List := FindAllFiles(Path, '', True);
    for i:=0 to List.Count-1 do
    begin
      Inc(Size, FileSize(List[i]));
    end;
    List.Free;
  end;
  Result := Size;
end;


{ OS.ReadFile(FileName:String): String }
function ReadFile(FileName: String): String; Cdecl;
var
  FileStream : TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
   if (FileStream.Size > 0) then
   begin
    SetLength(Result, FileStream.Size);
    FileStream.Read(Pointer(Result)^, FileStream.Size);
   end;
  finally
    FileStream.Free;
  end;
end;


{ OS.UTime(Path:String; Times:TIntArray = [CreationTime, LastAccessTime, LastWriteTime] }
function UTime(Path:String; Times:TIntArray): Boolean; Cdecl;
var
  H:THandle;
  ST1,ST2,ST3: TSystemTime;
  FT1,FT2,FT3: TFileTime;
begin
  if not Length(Times) = 3 then Exit;
  H := FileOpen(Path, fmOpenReadWrite);
  ST1 := EpochTimeToSysTime(Times[0]);
  ST2 := EpochTimeToSysTime(Times[1]);
  ST3 := EpochTimeToSysTime(Times[2]);
  SystemTimeToFileTime(ST1,FT1);
  SystemTimeToFileTime(ST2,FT2);
  SystemTimeToFileTime(ST3,FT3);
  try
    if (Times[0] > 0) and (Times[1] > 0) and (Times[2] > 0) then
      Result := SetFileTime(H, @FT1, @FT2, @FT3)
    else if (Times[0] <= 0) and (Times[1] > 0) and (Times[2] > 0) then
      Result := SetFileTime(H, nil, @FT2, @FT3)
    else if (Times[0] > 0) and (Times[1] <= 0) and (Times[2] > 0) then
      Result := SetFileTime(H, @FT1, nil, @FT3)
    else if (Times[0] > 0) and (Times[1] > 0) and (Times[2] <= 0) then
      Result := SetFileTime(H, @FT1, @FT2, nil)
    else if (Times[0] > 0) and (Times[1] <= 0) and (Times[2] <= 0) then
      Result := SetFileTime(H, @FT1, nil, nil)
    else if (Times[0] <= 0) and (Times[1] > 0) and (Times[2] <= 0) then
      Result := SetFileTime(H, nil, @FT2, nil)
    else if (Times[0] <= 0) and (Times[1] <= 0) and (Times[2] > 0) then
      Result := SetFileTime(H, nil, nil, @FT3);
      
    if not(Result) then RaiseLastOSError;
  finally 
    CloseHandle(H);
  end;
end;


procedure TouchFile(Path:String); Cdecl;
var
  H:THandle;
  ST: TSystemTime;
  FT: TFileTime;
begin
  H := FileOpen(Path, fmOpenReadWrite);
  GetSystemTime(ST);
  SystemTimeToFileTime(ST, FT);
  SetFileTime(H, nil, @FT, nil);
  CloseHandle(H);
end;


end.
