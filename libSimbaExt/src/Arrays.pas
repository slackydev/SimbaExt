unit Arrays;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Experimental & not properly tested yet.
 FPC + Generics = Iffy..
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode delphi}
{$inline on}

interface
uses
  CoreTypes, SysUtils;

const
  UNDEFINED = High(Int32);
  
type
  TDimensions = array [0..2] of SizeInt;

  TArrayStub<T> = class(TObject)
  public type
    TArrayData = array of T;
    PtrT = ^T;
    TCompareFunc = function(x,y:T): Int32;
  protected
    FData: TArrayData;
    FDims: UInt8;
    FSize: TDimensions;
  public
    Compare: TCompareFunc;

    constructor Create(data:TArrayData=nil; cmpFunc:TCompareFunc=nil; DoCopy:Boolean=False);
    destructor Destroy; override;

    procedure SetLen(newLen:SizeInt); inline;
    procedure SetLen(newLen:TDimensions); overload; inline;

    property Items: TArrayData read FData write FData;
    property Size: TDimensions read FSize;
    property Dimensions: UInt8 read FDims;
  end;


  (* 
    An implementation of a simple 1d array structure
  *)
  TArray<T> = class(TArrayStub<T>)
  public type
    DArrayStub = TArrayStub<T>;
  public
    function Length: SizeInt; inline;
    function High: SizeInt; inline;
    function Copy: DArrayStub;
    
    procedure Append(constref item:T); inline;
    procedure Extend(arr:DArrayStub); inline;
    function IndexOf(constref item:T): Int32;
    procedure Delete(idx:Int32); inline;
    procedure Remove(constref item:T); inline;
    function Pop(): T; inline;
    function Pop(idx:Int32): T; inline; overload;
    procedure Insert(idx:Int32; constref value:T);
    function Slice(start:Int32=UNDEFINED; stop:Int32=UNDEFINED; step:Int32=1): DArrayStub;
    procedure Sort(CompareFunc:TCompareFunc=nil);
  end;

  (*
    Specialize in numeric uses
  *)
  TNumericArray<T, TOvf> = class(TArray<T>)
  public type
    DArray = TArray<T>;
  public
    function Min(): T;
    function Max(): T;
    function Sum(): TOvf;
    function Mean(): Double;
    function Median(): T;
    function Variance(): Double;
    function StDev(): Double;
    function Mode(): T;
    function QuickSelect(kth:Int32; start:Int32=0; stop:Int32=-1): T;
  end;

  DynPointArray  = TArray<TPoint>;

  DynUInt8Array  = TNumericArray<UInt8, UInt32>;   DynByteArray = DynUInt8Array;
  DynInt32Array  = TNumericArray<Int32, Int64>;    DynIntArray  = DynInt32Array;
  DynInt64Array  = TNumericArray<Int64, Int64>;
  
  DynSingleArray = TNumericArray<Single, Double>;  DynFloatArray = DynSingleArray;
  DynDoubleArray = TNumericArray<Double, Double>;


//--------------------------------------------------
implementation
uses
  ExceptionMgr, Math, CoreMisc, CoreMath, Sorting;


constructor TArrayStub<T>.Create(data:TArrayData=nil; cmpFunc:TCompareFunc=nil; DoCopy:Boolean=False);
begin
  if (Pointer(data) <> nil) and DoCopy then
  begin
    SetLength(FData, System.Length(data));
    Move(data[0], FData[0], System.Length(data)*SizeOf(T));
  end else
    FData := data;

  FDims := 0;
  FSize[0] := System.Length(FData);

  Compare := cmpFunc;
end;


destructor TArrayStub<T>.Destroy;
begin
  SetLength(FData, 0);
  FSize[0] := 0;
  inherited;
end;

procedure TArrayStub<T>.SetLen(newLen:SizeInt);
begin
  if Dimensions = 0 then
  begin
    SetLength(FData, newLen);
    FSize[0] := newLen;
  end else
    NewException('Operation not supported');
end;

procedure TArrayStub<T>.SetLen(newLen:TDimensions); overload;
var
  i,bpr:Int32;
  tmp:TArrayData;
  oldLen: TDimensions;
begin
  if Dimensions = 0 then
  begin
    SetLength(FData, newLen[0]);
    FSize[0] := newLen[0];
  end else 
  if Dimensions = 1 then
  begin
    tmp := FData;
    oldLen := FSize;
    SetLength(FData, newLen[0]*newLen[1]);
    FSize := newLen;

    if oldLen[1] <= FSize[1] then
      bpr := SizeOf(T) * oldLen[1]
    else
      bpr := SizeOf(T) * FSize[1];

    for i:=0 to FSize[0] do
      Move(tmp[i*oldLen[1]], FData[i*FSize[1]], bpr);
  end
  else
    NewException('Operation not supported');
end;

//---| TArray |---------------------------------------------------------------\\
//----------------------------------------------------------------------------//

function TArray<T>.High: SizeInt;
begin
  Result := Size[0]-1;
end;


function TArray<T>.Length: SizeInt;
begin
  Result := Size[0];
end;

function TArray<T>.Copy(): DArrayStub;
begin
  Result := DArrayStub.Create(FData, nil, True);
end;


procedure TArray<T>.Append(constref item:T);
var L:Int32;
begin
  L := Size[0];
  Self.SetLen(L+1);
  FData[L] := item;
end;

procedure TArray<T>.Extend(arr:DArrayStub);
var len:Int32;
begin
  len := Size[0];
  Self.SetLen(arr.Size[0] + len);
  Move(Arr.Items[0], FData[len], Arr.Size[0] * SizeOf(T));
end;

function TArray<T>.IndexOf(constref item:T): Int32;
var i:Int32;
begin
  for i:=0 to Size[0]-1 do
    if FData[i] = item then
      Exit(i);
  Result := -1;
end;

procedure TArray<T>.Delete(idx:Int32);
begin
  if (Size[0] <= idx) or (idx < 0) then 
    Exit();
  if (Size[0]-1 <> idx) then
    Move(FData[idx+1], FData[idx], (Size[0]-idx)*SizeOf(T));
  Self.SetLen(Size[0]-1);
end;

procedure TArray<T>.Remove(constref item:T);
begin
  Self.Delete( IndexOf(item) ); 
end;

function TArray<T>.Pop(): T;
begin
  Result := FData[Size[0]-1];
  Self.SetLen(Size[0]-1);
end;

function TArray<T>.Pop(idx:Int32): T; overload;
begin
  Result := FData[idx];
  Delete(idx);
end;

procedure TArray<T>.Insert(idx:Int32; constref value:T);
var 
  l:Int32;
begin
  l := Size[0];
  if (idx < 0) then
    idx := modulo(idx,l);

  if (l <= idx) then begin
    self.Append(value);
    Exit();
  end;
  
  Self.SetLen(L+1);
  Move(FData[idx], FData[idx+1], (L-Idx)*SizeOf(T));
  FData[idx] := value;
end;

 
function TArray<T>.Slice(start:Int32=UNDEFINED; stop:Int32=UNDEFINED; step:Int32=1): DArrayStub;
var 
  P,R:PtrT;
  l:Int32; 
  H:PtrUInt;
begin
  if Step = 0 then 
    Exit;
  
  if (Start = UNDEFINED) then
    if Step < 0 then Start := -1
    else Start := 0;
  if (Stop = UNDEFINED) then
    if Step > 0 then Stop := -1
    else Stop := 0;

  h := self.Length;
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start, h);
  Stop  := Modulo(stop, h);

  if (Start > Stop) and (Step > 0) then
    NewException(exModuloFailure);

  Result := DArrayStub.Create(nil);
  Result.SetLen(((Stop-Start) div step)+1);
  P := @FData[start];
  R := @Result.Items[0];
  L := PtrUInt(R) + (SizeOf(T)*Result.Size[0]);
  while PtrUInt(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;

procedure TArray<T>.Sort(CompareFunc: TCompareFunc=nil);
var
  len,i,j:Int32;
  hi,m,k,left,right,upper:Int32;
  temp:TArrayData;
begin
  if (@CompareFunc = nil) then
     CompareFunc := Compare;

  len := Length();
  hi  := len-1;
  
  SetLength(temp, len);
  k := 1;
  while k < hi do
  begin
    left:=0;
    while left+k < len do
    begin
      right := left + k;
      upper := right + k;
      if upper > len then upper := len;

      m := left;
      i := left;
      j := right;

      while (i < right) and (j < upper) do
      begin
        if CompareFunc(FData[i], FData[j]) >= 0 then
        begin
          temp[m] := FData[i];
          Inc(i);
        end else
        begin
          temp[m] := FData[j];
          Inc(j);
        end;
        Inc(m);
      end;

      while (i < right) do begin
        temp[m] := FData[i];
        Inc(i);
        Inc(m);
      end;

      while (j < upper) do begin
        temp[m] := FData[j];
        Inc(j);
        Inc(m);
      end;

      Move(temp[left], FData[left], SizeOf(T)*(upper - left));
      Inc(left, 2*k);
    end;
    Inc(k,k);
  end;
end;

//---| TNumericArray |--------------------------------------------------------\\
//----------------------------------------------------------------------------//

function TNumericArray<T, TOvf>.Min(): T;
var i:Int32;
begin
  if Size[0] = 0 then Exit(0);
  Result := FData[0];
  for i:=1 to self.High do
    if FData[i] < Result then
       Result := FData[i];
end;

function TNumericArray<T, TOvf>.Max(): T;
var i:Int32;
begin
  if Size[0] = 0 then Exit(0);
  Result := FData[0];
  for i:=1 to self.High do
    if FData[i] > Result then
       Result := FData[i];
end;

function TNumericArray<T, TOvf>.Sum(): TOvf;
var i:Int32;
begin
  if self.Length = 0 then Exit(0);
  Result := FData[0];
  for i:=1 to self.High do Result += FData[i];
end;

function TNumericArray<T, TOvf>.Mean(): Double;
begin
  if self.Length = 0 then Exit(0);
  Result := Self.Sum() / self.Length;
end;

function TNumericArray<T, TOvf>.Median(): T;
begin
  if self.Length = 0 then Exit(0);
  Result := Self.QuickSelect(self.Length div 2);
end;

function TNumericArray<T, TOvf>.Variance(): Double;
var
  i:Int32;
  avg, square:Double;
begin
  avg := Self.Mean();
  square := 0;
  for i:=0 to self.High do
    square += (FData[i] - avg) * (FData[i] - avg);
  Result := square / self.Length;
end;

function TNumericArray<T, TOvf>.Stdev(): Double;
begin
  Result := Sqrt(Self.Variance());
end;

function TNumericArray<T, TOvf>.Mode(): T;
var
  i,hits,best:Int32;
  cur:T;
  arr:DArray;
begin
  arr := DArray.Create(FData, Compare, True);
  arr.Sort;
  cur := FData[0];
  hits := 1;
  best := 0;
  for i:=1 to arr.High do
  begin
    if (cur <> arr.items[i]) then
    begin
      if (hits > best) then
      begin
        best := hits;
        Result := cur;
      end;
      hits := 0;
      cur := arr.items[i];
    end;
    Inc(hits);
  end;
  if (hits > best) then
    Result := cur;

  Arr.Free;
end;

function TNumericArray<T, TOvf>.QuickSelect(kth:Int32; start:Int32=0; stop:Int32=-1): T;
var
  l,r:Int32;
  tmp,mid:T;
begin
  if stop = -1 then stop := Self.High;
  if stop-start < 0 then Exit();

  while (start < stop) do
  begin
    l := start;
    r := stop;
    mid := FData[(l + r) div 2];
    while (l < r) do
      if (FData[l] >= mid) then
      begin
        tmp := FData[r];
        FData[r] := FData[l];
        FData[l] := tmp;
        Dec(r);
      end else
        Inc(l);

    if (FData[l] > mid) then Dec(l);
    if (kth <= l) then stop := l else start := l + 1;
  end;

  Result := FData[kth];
end;


end.
