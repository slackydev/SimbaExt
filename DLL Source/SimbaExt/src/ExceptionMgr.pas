Unit ExceptionMgr;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface

uses SysUtils,CoreTypes;

var Errors:TStringArray;
const EMaxErrors = 32;

function PopException(var Msg:String): Boolean; cdecl; 
procedure NewException(Msg:String);
procedure NewExceptionFmt(Msg:String; Args:Array of Const);

 
//--------------------------------------------------
implementation
uses CoreMath;

procedure RaiseException(Msg:String);
var E: Exception;
begin
  Raise E.Create(Msg);
end; 


function PopException(var Msg:String): Boolean; cdecl;
begin
  Result := False;
  if Length(Errors) <> 0 then 
  begin
    Msg := Errors[High(Errors)];
    SetLength(Errors, High(Errors));
    Result := True;
  end;
end;


procedure NewException(Msg:String);
var i:Int32;
begin
  if Length(Errors) = EMaxErrors then 
  begin
    for i:=1 to High(Errors) do
      Errors[i-1] := Errors[i];
    Errors[High(Errors)] := Msg;
  end else
  begin
    SetLength(Errors, Length(Errors)+1);
    Errors[High(Errors)] := Msg;
  end;
  RaiseException(Msg);
end;


procedure NewExceptionFmt(Msg:String; Args:Array of Const);
begin
  NewException(Format(Msg, Args));
end;

end.







