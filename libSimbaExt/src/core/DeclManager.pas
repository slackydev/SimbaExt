unit DeclManager;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$inline on}

interface
uses
  Classes, SysUtils, Dictionary;

type
  TManagedClasses = specialize TDictionary<NativeInt, TObject>;
  TScriptThreads  = specialize TDictionary<NativeInt, TManagedClasses>;
  
  TManagedDeclarations = class(TObject)
    Threads: TScriptThreads;

    constructor Create();
    procedure Add(thread: NativeInt; decl:TObject);
    procedure Release(thread: NativeInt; decl:TObject);
    function Exists(thread: NativeInt; decl:TObject): Boolean;
    procedure ReleaseThread(thread: NativeInt);
  end;
  

//--------------------------------------------------
implementation

uses Arrays;

constructor TManagedDeclarations.Create();
begin
  Threads :=  TScriptThreads.Create(@HashNative);
end;

procedure TManagedDeclarations.Add(thread: NativeInt; decl:TObject);
var DeclList:TManagedClasses;
begin
  DeclList := Threads.GetDef(thread, nil);
  if (DeclList = nil) then 
  begin
    DeclList := TManagedClasses.Create(@HashNative);
    Threads[thread] := DeclList;
  end;
  DeclList[PtrInt(Pointer(Decl))] := Decl;
end;


procedure TManagedDeclarations.Release(thread: NativeInt; decl:TObject);
var 
  DeclList:TManagedClasses;
  obj: TObject;
begin
  DeclList := Threads.GetDef(thread, nil);
  if (DeclList <> nil) then 
  begin
    obj := DeclList.GetDef(PtrInt(Pointer(Decl)), nil);
    if obj <> nil then
    begin
      obj.Free();
      DeclList.Remove(PtrInt(Pointer(Decl)));
    end;
  end;
end;


function TManagedDeclarations.Exists(thread: NativeInt; decl:TObject): Boolean;
var 
  DeclList: TManagedClasses;
begin
  Result := False;
  DeclList := Threads.GetDef(thread, nil);
  if (DeclList <> nil) then 
    Result := DeclList.GetDef(PtrInt(Pointer(Decl)), nil) <> nil;
end;


procedure TManagedDeclarations.ReleaseThread(thread: NativeInt);
var 
  DeclList: TManagedClasses;
  i,j: Int32;
begin
  DeclList := Threads.GetDef(thread, nil);
  if (DeclList <> nil) then 
    for i:=0 to High(DeclList.Items) do
      for j:=0 to High(DeclList.Items[i]) do
        DeclList.Items[i][j].val.Free();
  Threads.Remove(thread);
end;

end.
