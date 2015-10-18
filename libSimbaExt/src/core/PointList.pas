Unit PointList;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(* 
  Simplefy inserting, appending, popping etc when working with TPoints. While 
  still keeping optimal speed.
*)
{$mode objfpc}{$H+}
{$macro on}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  CoreTypes, Math;

const
  LMINSIZE = 4096;

type
  TPointList = object
  private
    FHigh: Integer;
    FLength: Integer;
    FArr: TPointArray;
    function Get(Index:Integer): TPoint; Inline;
    procedure Put(Index:Integer; const Value: TPoint); Inline;
  public
    property Items: TPointArray read FArr write FArr;
    
    (*
     Initalize
    *)
    procedure Init;

    
    (*
     Initalize with your own TPA
    *)
    procedure InitWith(TPA:TPointArray);

    
    (*
     Release the array
    *)
    procedure Free;

    
    (*
     Indexing the array.
    *)
    property Point[Index : Integer]: TPoint read Get write Put; default;
    
    
    (*
     Same as initlaize, tho the pourpose is not the same.
    *)
    procedure Reset; Inline;

    (*
     Check if we can give it a new size.. It's manly used internally, but can be used elsewere..
     Note: NewSize = The highest index. (Not same as the normal SetLength).
    *)
    procedure CheckResize(NewSize:Integer); Inline;


    (*
     Check if it's time to give it a new upper size.. Faster then calling CheckResize.
    *)
    procedure CheckResizeHigh(NewSize:Integer); Inline;


    (*
     Check if it's time to give it a new lower size.. Faster then calling CheckResize.
    *)
    procedure CheckResizeLow(NewSize:Integer); Inline;

    
    (*
     Returns a copy of the TPA/Array.
    *)
    function Clone: TPointList; inline;
    

    (*
     Returns a (partial) copy of the TPA/Array.
    *)
    procedure CopyTo(var Arr:TPointArray; Start, Stop: Integer); Inline;


    (*
     Returns the last item
    *)
    function Peek: TPoint; Inline;
    

    (*
     Remove the last item, and return what it was. It also downsizes the array if
     possible.
    *)
    function Pop: TPoint; Inline;


    (*
     Remove the last item, and return what it was.
    *)
    function FastPop: TPoint; Inline;


    (*
     Insert a item at the given position. It resizes the array if needed.
    *)
    procedure Insert(const Item: TPoint; Index:Integer); Inline;


    (*
     Appends the item to the last position in the array. Resizes if needed.
    *)
    procedure Append(const Item: TPoint); Inline;


    (*
     Appends the item to the last position in the array. Resizes if needed.
    *)
    procedure Append(const X,Y: Integer); Inline; overload;
    
    
    (*
     Extend the current array with the given TPA.
    *)
    procedure Extend(const TPA: TPointArray); Inline;

    
    (*
     Remove the first item from the lists whos value is `Value`.
    *)
    procedure Remove(const Item: TPoint); Inline;

    
    (*
     Remove the given index from the array.
    *)
    procedure Delete(const Index: Integer); Inline;
    
    
    (*
     Remove the given indices from the array.
    *)
    procedure DeleteEx(const Indices: TIntArray); Inline;
    
    
    (*
     Offset each point in the list by X,Y.
    *)
    procedure Offset(X,Y:Integer); Inline;
    
    
    (*
     Swaps to items of specified indexes. 
    *)
    procedure Swap(Index1, Index2: Integer); Inline;
    
    
    (*
     Check if the array is empty or not.
    *)
    function IsEmpty: Boolean; Inline;


    (*
     Check if the array has items or not.
    *)
    function NotEmpty: Boolean; Inline;


    (*
     Returns the length of the array including the overhead.
    *)
    function GetLength: Integer; Inline;


    (*
     Returns the size, in a way it's the same as `Length(Arr)`.
    *)
    function GetSize: Integer; Inline;


    (*
     Returns the highest index, in a way it's the same as `High(Arr)`.
    *)
    function GetHigh: Integer; Inline;


    (*
     It sets the overhead length down to the highest index + 1.
     It's used before a call to an external function, EG before using: GetTPABounds.
    *)
    function Finalize: TPointArray;
  end;


//--------------------------------------------------
implementation


procedure TPointList.Init;
begin
  FHigh := -1;
  FLength := LMINSIZE;
  SetLength(FArr, FLength);
end;


procedure TPointList.InitWith(TPA:TPointArray);
begin
  FArr := TPA;
  FHigh := High(FArr);
  FLength := Length(FArr);
  if FLength < LMINSIZE then
  begin
    FLength := LMINSIZE;
    SetLength(FArr, FLength);
  end;
end;


procedure TPointList.Free;
begin
  FHigh := -1;
  FLength := 0;
  SetLength(FArr, 0);
end;


procedure TPointList.Reset;
begin
  FHigh := -1;
  FLength := LMINSIZE;
  SetLength(FArr, FLength);
end;


procedure TPointList.CheckResize(NewSize:Integer);
begin
  if NewSize < LMINSIZE then
  begin
    if  FLength > LMINSIZE then
      SetLength(FArr, LMINSIZE);
    FLength := LMINSIZE;
    FHigh := NewSize;
    Exit;
  end;
  FHigh := NewSize;
  case (FHigh >= FLength) of
   False:
    if ((FLength div 2) > FHigh) then
    begin
      FLength := FLength div 2;
      SetLength(FArr, FLength);
    end;
   True:
    begin
      FLength := FLength + FLength;
      SetLength(FArr, FLength);
    end;
  end;
end;


procedure TPointList.CheckResizeHigh(NewSize:Integer);
begin
  FHigh := NewSize;
  if (FHigh >= FLength) then
  begin
    FLength := FLength + FLength;
    SetLength(FArr, FLength);
  end;
end;


procedure TPointList.CheckResizeLow(NewSize:Integer);
begin
  if NewSize < LMINSIZE then
  begin
    if  FLength > LMINSIZE then
      SetLength(FArr, LMINSIZE);
    FLength := LMINSIZE;
    FHigh := NewSize;
    Exit;
  end;

  FHigh := NewSize;
  if ((FLength div 2) > FHigh) then
  begin
    FLength := FLength div 2;
    SetLength(FArr, FLength);
  end;
end;


function TPointList.Clone: TPointList;
begin
  Result := self;
  Result.Items := Copy(FArr, 0, FLength)
end;


procedure TPointList.CopyTo(var Arr:TPointArray; Start, Stop: Integer);
var i:Integer;
begin
  if FHigh > -1 then
  begin
    Stop := Min(Stop, FHigh);
    SetLength(Arr, (Stop - Start) + 1);
    for i := Start to Stop do
      Arr[i-Start] := FArr[i];
  end;
end;


function TPointList.Peek: TPoint;
begin
  Result := FArr[FHigh];
end;


function TPointList.Pop: TPoint;
begin
  Result := FArr[FHigh];
  Dec(FHigh);
  CheckResizeLow(FHigh);
end;


function TPointList.FastPop: TPoint;
begin
  Result := FArr[FHigh];
  Dec(FHigh);
end;



procedure TPointList.Insert(const Item: TPoint; Index:Integer);
var i:Integer;
begin
  CheckResizeHigh(FHigh+1);
  if Index > FHigh then //Remove old crap.. and resize
  begin
    SetLength(FArr, FHigh); 
    CheckResizeHigh(Index);
  end;
  for i:=FHigh-1 downto Index do
    FArr[i+1] := FArr[i];
  FArr[Index] := Item;
end;


procedure TPointList.Append(const Item: TPoint);
begin
  Inc(FHigh);
  CheckResizeHigh(FHigh);
  FArr[FHigh] := Item;
end;


procedure TPointList.Append(const X,Y: Integer);
begin
  Inc(FHigh);
  CheckResizeHigh(FHigh);
  FArr[FHigh].x := X;
  FArr[FHigh].y := Y;
end;


procedure TPointList.Extend(const TPA: TPointArray);
var
 i,_h,h:Integer;
begin
  H := Length(TPA);
  if (H = 0) then Exit;
  _h := FHigh + 1;
  CheckResizeHigh(H + _h);
  for i := 0 to H-1 do
    FArr[i+_h] := TPA[i];
end;


procedure TPointList.Remove(const Item: TPoint);
var
 i,j:Integer;
 hit:Boolean;
begin
  if (FHigh = -1) then Exit;

  hit := False;
  j := 0;
  for i := 0 to FHigh do
  begin
    if Not(hit) and (FArr[i].X = Item.X) and (FArr[i].Y = Item.Y) then
    begin
      hit := True;
      j := i;
    end else if (hit) then
    begin
      FArr[j] := FArr[i];
      Inc(j);
    end;
  end;

  if (hit) then
    CheckResizeLow(FHigh - 1);
end;


procedure TPointList.Delete(const Index: Integer);
var
 i,j:Integer;
begin
  if (FHigh = -1) then Exit;
  if (Index > FHigh) or (Index < 0) then Exit;
  j := 0;
  
  for i:=Index to FHigh do
  begin
    if (i=Index) then
    begin
      j := i;
    end else
    begin
      FArr[j] := FArr[i];
      Inc(j);
    end;
  end; 

  CheckResizeLow(FHigh - 1);
end;


procedure TPointList.DeleteEx(const Indices: TIntArray);
var
 i,len,lo:Integer;
 Del:TBoolArray;
begin
  if (FHigh = -1) then Exit;
  Lo := FHigh;
  SetLength(Del, FHigh+1);
  for i := 0 to High(Indices) do
    if ((Indices[i] > -1) and (Indices[i] <= FHigh)) then
    begin
      Del[Indices[i]] := True;
      if (Indices[i] < Lo) then Lo := Indices[i];
    end;

  len := 0;
  for i:=Lo to FHigh do
    if not(Del[i]) then
    begin
      FArr[len] := FArr[i];
      Inc(len);
    end;

  SetLength(Del, 0);
  CheckResizeLow(FHigh - (High(Indices)-len));
end;


procedure TPointList.Offset(X,Y:Integer);
var i: Integer;
begin
  if (FHigh = -1) then Exit;
  for i:=0 to FHigh do
  begin
    FArr[i].x := FArr[i].x + X;
    FArr[i].y := FArr[i].y + Y;
  end;
end;



//Private - Use: TPointList.Point[Index];
function TPointList.Get(Index:Integer): TPoint;
begin
  Result := FArr[Index];
end;


//Private - Use: TPointList.Point[Index] := Value;
procedure TPointList.Put(Index:Integer; const Value: TPoint);
begin
  FArr[Index] := Value;
end;


procedure TPointList.Swap(Index1, Index2: Integer);
var Tmp:TPoint;
begin
  Tmp := FArr[Index1];
  FArr[Index1] := FArr[Index2];
  FArr[Index2] := Tmp;
end;


function TPointList.IsEmpty: Boolean;
begin
  Result := FHigh < 0;
end;


function TPointList.NotEmpty: Boolean;
begin
  Result := FHigh > -1;
end;


function TPointList.GetLength: Integer;
begin
  Result := FLength;
end;


function TPointList.GetHigh: Integer;
begin
  Result := FHigh;
end;


function TPointList.GetSize: Integer;
begin
  Result := FHigh + 1;
end;


function TPointList.Finalize: TPointArray;
begin
  FLength := FHigh + 1;
  SetLength(FArr, FLength);
  Result := FArr;
end;


end.
