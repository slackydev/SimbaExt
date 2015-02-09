Unit ExceptionMgr;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
interface
uses SysUtils,CoreTypes;

var Errors:TStringArray;
const EMaxErrors = 32;

function PopException(var Msg:String): LongBool; cdecl;
function exp_PopException(SE:Pointer; out Msg:String): LongBool; cdecl;
procedure NewException(Msg:String);
procedure NewExceptionFmt(Msg:String; Args:Array of Const);

resourcestring
  exEmptyMatrix = 'An empty matrix is given';
  exEmptyArray = 'An empty array is given';
  exModuloFailure = 'Bad parameters passed to modulo';
  exEmptyBox = 'Empty box passed to %s';

 
//--------------------------------------------------
implementation
uses CoreMath;

procedure RaiseException(Msg:String);
var E: Exception;
begin
  WriteLn('Error: '+Msg);
  Raise E.Create(Msg);
end; 


function PopException(var Msg:String): LongBool; cdecl;
begin
  Result := False;
  if Length(Errors) <> 0 then 
  begin
    Msg := Errors[High(Errors)];
    SetLength(Errors, High(Errors));
    Result := True;
  end;
end;


function exp_PopException(SE:Pointer; out Msg:String): LongBool; cdecl;
begin
  PopException(Msg);
end;


procedure NewException(Msg:String);
var i,Len:Int32;
begin
  Len := Length(Errors);
  if Len = EMaxErrors then
  begin
    for i:=1 to Len-1 do
      Errors[i-1] := Errors[i];
    Errors[Len-1] := Msg;
  end else
  begin
    SetLength(Errors, Len+1);
    Errors[Len] := Msg;
  end;
  RaiseException(Msg);
end;


procedure NewExceptionFmt(Msg:String; Args:Array of Const);
begin
  NewException(Format(Msg, Args));
end;

end.







