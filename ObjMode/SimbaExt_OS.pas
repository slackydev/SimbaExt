{*=========================================================================================|
| OS.pas                                                                                   |
|=========================================================================================*}

function TObjOS.GetEnviron(Varname:String): String;
begin
  Result := exp_GetEnviron(Varname);
end;

  
function TObjOS.SetEnviron(Varname, Value:String): Boolean;
begin
  Result := exp_SetEnviron(Varname, Value);
end;


{#DOCUMENT} {
  [method]function OS.ListDir(Dir:String): TStringArray;[/method]
  [desc]
    Lists all the files and folders in the given directory
    [code=pascal]...[/code]
  [/desc]
  [keywords]OS, OS.ListDir, ListDir[/keywords]
}{#END}
function TObjOS.ListDir(Dir:String): TStringArray;
begin
  exp_ListDir(Dir, Result);
end;

function TObjOS.Remove(FileName:String): Boolean;
begin
  Result := exp_FileRemove(FileName);
end;

function TObjOS.RmDir(Dir:String): Boolean;
begin
  Result := exp_DirRemove(Dir);
end;

function TObjOS.Rename(Src, Dest:String): Boolean;
begin
  Result := exp_FileRename(Src, Dest);
end;  

function TObjOS.CopyFile(Src, Dest:String): Boolean;
begin
  Result := exp_FileRename(Src, Dest);
end;  


function TObjOS.Size(Loc:String): Int64;
begin
  Result := exp_FileSize2(Loc);
end;


function TObjOS.FileSize(F:String): Int64;
begin
  Result := -1;
  if OS.Path.IsFile(F) then
    Result := exp_FileSize2(F);
end;

function TObjOS.ReadFile(FileName: String): String;
begin
  if OS.Path.IsFile(FileName) then
    Result := exp_ReadFile(FileName);
end;


function TObjOS.UTime(F:String; Times:TIntArray): Boolean;
begin
  Result := exp_UTime(F,Times);
end;


procedure TObjOS.Touch(F:String);
begin
  exp_TouchFile(F);
end;


// OS.Path.* \\------------------------------------------------------||

function TObjOSPath.NormPath(Loc:String): String;
begin
  Result := exp_NormPath(Loc);
end;

function TObjOSPath.AbsPath(Loc:String): String;
begin
  Result := exp_AbsPath(Loc);
end;

function TObjOSPath.DirName(Loc:String): String;
begin
  Result := exp_DirName(Loc);
end;

function TObjOSPath.Exits(Loc:String): Boolean;
begin
  Result := exp_PathExists(Loc);
end;

function TObjOSPath.IsAbs(Loc:String): Boolean;
begin
  Result := exp_IsAbsPath(Loc);
end;

function TObjOSPath.IsFile(Loc:String): Boolean;
begin
  Result := exp_IsFile(Loc);
end;


function TObjOSPath.IsDir(Loc:String): Boolean;
begin
  Result := exp_IsDir(Loc);
end;
