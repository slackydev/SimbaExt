Unit ThreadPool;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline on}
     
interface
uses
  CoreTypes, SysUtils, Classes;

type
  TThreadMethod = procedure(params:PParamArray); 
  TThreadId = Int32;
  
  TExecThread = class(TThread)
  protected
    FMethod: TThreadMethod;
    FParams: TParamArray;
    procedure Execute; override;
  public
    Executed: Boolean;
    constructor Create();
    procedure SetMethod(Method: TThreadMethod); inline;
    procedure SetArgument(argId:Int32; arg:Pointer); inline;
  end;

  TThreadArray = Array [0..1500] of record
    Thread: TExecThread;
    Available: Boolean;
    Initialized: Boolean;
  end;


  TThreadPool = record
    threads: TThreadArray;
    function GetAvailableThread(): TThreadId;
    function  NewThread(method: TThreadMethod): TThreadId;
    procedure SetArgument(t:TThreadId; argId:Int32; arg:Pointer); inline;
    procedure SetArguments(t:TThreadId; args:array of Pointer);
    procedure Start(t:TThreadId);
    function  Executed(t:TThreadId): Boolean; inline;
    procedure Free(t:TThreadId);
  end;

var 
  Pool: TThreadPool;

procedure MatrixFunc(Method:TThreadMethod; Args: Array of Pointer; W,H:Int32; nThreads:UInt8=0; fallback:Int32=50000);

  
//--------------------------------------------------
implementation

uses math, fileutil, ExceptionMgr;


(*----| WorkThread |----------------------------------------------------------*)
constructor TExecThread.Create();
begin
  FreeOnTerminate := True;
  Executed := False;
  inherited Create(True);
end;

procedure TExecThread.Execute;
begin
  FMethod(@FParams);
  Executed := True;
end;

procedure TExecThread.SetMethod(Method: TThreadMethod);
begin
  FMethod := Method;
end;

procedure TExecThread.SetArgument(argId:Int32; arg:Pointer);
begin
  Self.FParams[argId] := arg;
end;


(*----| ThreadPool |----------------------------------------------------------*)
function TThreadPool.GetAvailableThread(): TThreadId;
var i:Int32;
begin
  for i:=0 to High(Threads) do
    if Threads[i].Available then
      Exit(TThreadId(i));
  raise Exception.Create('TThreadPool.GetAvailableThread: No free execution threads'); 
end;

function TThreadPool.NewThread(method: TThreadMethod): TThreadId;
begin
  result := GetAvailableThread();
  if not(Threads[result].Thread = nil) then
    Threads[result].Thread.Terminate();
  Threads[result].Thread := TExecThread.Create();
  Threads[result].Available := False;
  Threads[result].Thread.SetMethod(method);
end;

procedure TThreadPool.SetArgument(t:TThreadId; argid:Int32; arg:Pointer);
begin
  Threads[t].thread.SetArgument(argid, arg);
end;

procedure TThreadPool.SetArguments(t:TThreadId; args:array of Pointer);
var arg:Int32;
begin
  for arg:=0 to High(args) do
    Threads[t].thread.SetArgument(arg, args[arg]);
end;

procedure TThreadPool.Start(t:TThreadId);
begin
  Threads[t].Thread.Start;
end;

function TThreadPool.Executed(t:TThreadId): Boolean;
begin
  if (Threads[t].Thread = nil) or (Threads[t].Available) then
     Result := False
  else
    Result := Threads[t].Thread.Executed = True;
end;

procedure TThreadPool.Free(t:TThreadId);
begin
  Threads[t].Available := True;
  Threads[t].Thread.Terminate;
  Threads[t].Thread := nil;
end;



procedure MatrixFunc(Method:TThreadMethod; Args: Array of Pointer; W,H:Int32; nThreads:UInt8=0; fallback:Int32=50000);
var
  i,lo,hi,dev: Int32;
  threads:Array of record id:TThreadId; box:TBox; end;
  params:TParamArray;
  area:TBox;
begin
  if (W*H < fallback) then
  begin
    area := Box(0,0,W-1,H-1);
    params := args;
    params[length(args)] := @area;
    Method(@params);
    Exit();
  end;

  if nThreads = 0 then
    {$IFDEF MSWINDOWS}
      nThreads := StrToIntDef(GetEnvironmentVariableUTF8('NUMBER_OF_PROCESSORS'), 4);
    {$ELSE}
      nThreads := 4;
    {$ENDIF}

  nThreads := Min(H, nThreads);
  SetLength(threads, nThreads);
  lo := 0;
  dev := H div nThreads;
  for i:=1 to nThreads do
  begin
    if (i = nThreads) then hi := H-1
    else                   hi := i * dev;

    threads[i-1].box := Box(0,lo,w-1,hi);
    threads[i-1].id := Pool.NewThread(Method);
    Pool.SetArguments(threads[i-1].id, Args);
    Pool.SetArgument(threads[i-1].id, length(args), @threads[i-1].box);
    Pool.Start(threads[i-1].id);

    lo := hi + 1;
  end;

  for i:=0 to nThreads-1 do
  begin
    while not Pool.Executed(threads[i].id) do Sleep(0);
    Pool.Free(threads[i].id);
  end;
end;


var __i:Int32;
initialization
  for __i:=0 to High(Pool.threads) do
    Pool.threads[__i].Available := True;
end.
