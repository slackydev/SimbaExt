{!DOCTOPIC}{ 
  Miscellaneous OS functions
}

{!DOCREF} {
  @method: var OS = TObjOS;
  @desc: 
    This module provides you with a few operating system dependent functions.
    
    [note]Note that SimbaExt is so far only build for Windows. This module will be modified to be cross-platform if linux ever where to be supported.[/note]
}



{!DOCREF} {
  @method: function OS.GetEnviron(Varname:String): String;
  @desc: 
    Call GetEnviron to retrieve the value of an environment variable
    [code=pascal]
    begin
      WriteLn(GetEnviron('PATH'));
    end.
    [/code]
}
function TObjOS.GetEnviron(Varname:String): String;
begin
  Result := exp_GetEnviron(Varname);
end;


{!DOCREF} {
  @method: function OS.SetEnviron(Varname, Value:String): Boolean;
  @desc: Call SetEnviron to set the value of an environment variable
}
function TObjOS.SetEnviron(Varname, Value:String): Boolean;
begin
  Result := exp_SetEnviron(Varname, Value);
end;


{!DOCREF} {
  @method: function OS.ListDir(Dir:String): TStringArray;
  @desc: 
    Lists all the files and folders in the given directory
    [code=pascal]WriteLn(OS.ListDir('C:\'));[/code]
}
function TObjOS.ListDir(Dir:String): TStringArray;
begin
  exp_ListDir(Dir, Result);
end;


{!DOCREF} {
  @method: function OS.Remove(FileName:String): Boolean;
  @desc: Remove (delete) the file path.
}
function TObjOS.Remove(FileName:String): Boolean;
begin
  Result := exp_FileRemove(FileName);
end;


{!DOCREF} {
  @method: function OS.RmDir(Dir:String): Boolean;
  @desc: Remove (delete) the directory path.
}
function TObjOS.RmDir(Dir:String): Boolean;
begin
  Result := exp_DirRemove(Dir);
end;


{!DOCREF} {
  @method: function OS.Rename(Src, Dest:String): Boolean;;
  @desc: Rename the file or directory src to dst
}
function TObjOS.Rename(Src, Dest:String): Boolean;
begin
  Result := exp_FileRename(Src, Dest);
end;  


{!DOCREF} {
  @method: function OS.CopyFile(Src, Dest:String): Boolean;
  @desc: Copys the file or directory src to dst
}
function TObjOS.CopyFile(Src, Dest:String): Boolean;
begin
  Result := exp_FileRename(Src, Dest);
end;  



{!DOCREF} {
  @method: function OS.Size(Loc:String): Int64;
  @desc: Returns the size of the file or directory.
}
function TObjOS.Size(Loc:String): Int64;
begin
  Result := exp_FileSize2(Loc);
end;


{!DOCREF} {
  @method: function OS.FileSize(F:String): Int64;
  @desc: Returns the size of the file.
}
function TObjOS.FileSize(F:String): Int64;
begin
  Result := -1;
  if OS.Path.IsFile(F) then
    Result := exp_FileSize2(F);
end;


{!DOCREF} {
  @method: function OS.ReadFile(FileName: String): String;
  @desc: 
    Reads a file directly in to a string.
    [code=pascal]
    var Str:String;
    begin
      Str := os.readfile(os.path.abspath('settings.xml'));
      WriteLn(str);
    end.
    [/code]
}
function TObjOS.ReadFile(FileName: String): String;
begin
  if OS.Path.IsFile(FileName) then
    Result := exp_ReadFile(FileName);
end;


{!DOCREF} {
  @method: function OS.UTime(F:String; Times:TIntArray): Boolean;
  @desc: 
    Set the 'created', 'access' and 'modificated' times of the file specified by path. If times is 0, then the file’s access and modified times is not modified.
    [code=pascal]
    var tnow: Int32;
    begin
      tnow := Trunc(TimeUtils.Time());
      OS.UTime('somefile.txt', [0,tnow,tnow]);
    end.
    [/code]
}
function TObjOS.UTime(F:String; Times:TIntArray): Boolean;
begin
  Result := exp_UTime(F,Times);
end;


{!DOCREF} {
  @method: procedure OS.Touch(F:String);
  @desc: Updates the access time of the given file path.
}
procedure TObjOS.Touch(F:String);
begin
  exp_TouchFile(F);
end;


// OS.Path.* \\------------------------------------------------------||

{!DOCREF} {
  @method: function OS.Path.NormPath(Loc:String): String;
  @desc: Normalize a pathname by collapsing redundant separators and up-level references so that A//B, A/B/, A/./B and A/foo/../B all become A/B.
}
function TObjOSPath.NormPath(Loc:String): String;
begin
  Result := exp_NormPath(Loc);
end;

{!DOCREF} {
  @method: function OS.Path.AbsPath(Loc:String): String;
  @desc: Return a normalized absolutized version of the pathname 'loc'.
}
function TObjOSPath.AbsPath(Loc:String): String;
begin
  Result := exp_AbsPath(Loc);
end;

{!DOCREF} {
  @method: function OS.Path.DirName(Loc:String): String;
  @desc: Return the directory name of pathname 'loc'. This is the first element of the pair returned by passing 'loc' to the function split().
}
function TObjOSPath.DirName(Loc:String): String;
begin
  Result := exp_DirName(Loc);
end;

{!DOCREF} {
  @method: function OS.Path.Exists(Loc:String): Boolean;
  @desc: Return True if 'loc' refers to an existing path.
}
function TObjOSPath.Exists(Loc:String): Boolean;
begin
  Result := exp_PathExists(Loc);
end;

{!DOCREF} {
  @method: function OS.Path.IsAbs(Loc:String): Boolean;
  @desc: Return True if path is an absolute pathname, meaning it should begin with a drive letter.
}
function TObjOSPath.IsAbs(Loc:String): Boolean;
begin
  Result := exp_IsAbsPath(Loc);
end;


{!DOCREF} {
  @method: function OS.Path.IsFile(Loc:String): Boolean;
  @desc: Return True if path refers to an existing file
}
function TObjOSPath.IsFile(Loc:String): Boolean;
begin
  Result := exp_IsFile(Loc);
end;


{!DOCREF} {
  @method: function OS.Path.IsDir(Loc:String): Boolean;
  @desc: Return True if path refers to an existing dir
}
function TObjOSPath.IsDir(Loc:String): Boolean;
begin
  Result := exp_IsDir(Loc);
end;
