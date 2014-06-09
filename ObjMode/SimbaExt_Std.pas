{!DOCTOPIC}{ Standard functions }


{!DOCREF} {
  @method: function se.Reduce(Func:Pointer; Arr:<1d array type>): <data type>;
  @desc: 
    Returns a single value constructed by calling the function `func` on the first two items of the sequence, then on the result 
    and the next item, and so on.[br]
    
    For example, to compute the sum of the numbers 1 through 10:
    [code=pascal]
    function F(x,y:Int32): Int32; 
    begin Result := x + y; end; 
    [br]
    WriteLn( se.Reduce(@F, [1,2,3,4,5,6,7,8,9,10]) );
    [/code]
    Output: `55`
    
    [params]
    [b]Func:[/b] function that takes two parameters (same type as array items), and returns the same type as the array items.
    [b]Arr:[/b] TByteArray, TIntArray, TFloatArray, TDoubleArray, TExtArray, TPointArray, TBoxArray
    [b]func64:[/b] [Byte, Int, Single]: if `True` then params of the given functions must be 64bit, meaning `Double`, or `Int64`, used to avoid overflowing.
    [/params]
}
{$IFNDEF CODEINSIGHT}
type
  __TReduceTBtA = function (x,y:Byte): Int64;
  __TReduceTIA  = function (x,y:Int32): Int64;
  __TReduceTFA  = function (x,y:Single): Extended;
  __TReduceTDA  = function (x,y:Double): Extended;
  __TReduceTEA  = function (x,y:Extended): Extended;
  __TReduceTPA  = function (x,y:TPoint): TPoint;
  __TReduceTBA = function (x,y:TBox): TBox;
  
  __TReduceTBtA64 = function (x,y:Int64): Int64;
  __TReduceTIA64  = function (x,y:Int64): Int64;
  __TReduceTFA64  = function (x,y:Single): Extended;
{$ENDIF}


//---| TBtA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TByteArray; func64:Boolean=False): Int64; overload;
var
  i,l:Int32; 
  Def:__TReduceTBtA;
  Def2: __TReduceTBtA64;
begin
  l := High(Arr);
  if l < 0 then Exit(0);
  if l = 0 then Exit(Arr[0]);
  case func64 of
  False: 
    begin
      Def := Func;
      Result := Def(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def(Result,Arr[i]);
    end;
  True:
    begin
      Def2 := Func;
      Result := Def2(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def2(Result,Arr[i]);
    end;
  end;
end;


//---| TIA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TIntArray; func64:Boolean=False): Int64; overload; {inline}
var
  i,l:Int32; 
  Def:__TReduceTIA;
  Def2: __TReduceTIA64;
begin
  l := High(Arr);
  if l < 0 then Exit(0);
  if l = 0 then Exit(Arr[0]);
  case func64 of
  False: 
    begin
      Def := Func;
      Result := Def(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def(Result,Arr[i]);
    end;
  True:
    begin
      Def2 := Func;
      Result := Def2(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def2(Result,Arr[i]);
    end;
  end;
end;


//---| TFA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TFloatArray; func64:Boolean=False): Double; overload;
var
  i,l:Int32; 
  Def:__TReduceTFA;
  Def2:__TReduceTFA64;
begin
  l := High(Arr);
  if l < 0 then Exit(0);
  if l = 0 then Exit(Arr[0]);
  case func64 of
  False: 
    begin
      Def := Func;
      Result := Def(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def(Result,Arr[i]);
    end;
  True:
    begin
      Def2 := Func;
      Result := Def2(Arr[0],Arr[1]);
      for i:=2 to High(Arr) do
        Result := Def2(Result,Arr[i]);
    end;
  end;
end;


//---| TDA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TDoubleArray): Extended; overload;
var
  i,l:Int32; Def:__TReduceTDA;
begin
  l := High(Arr);
  if l < 0 then Exit(0);
  if l = 0 then Exit(Arr[0]);
  Def := Func;
  Result := Def(Arr[0],Arr[1]);
  for i:=2 to High(Arr) do
    Result := Def(Result,Arr[i]);
end;


//---| TEA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TExtArray): Extended; overload;
var
  i,l:Int32; Def:__TReduceTEA;
begin
  l := High(Arr);
  if l < 0 then Exit(0);
  if l = 0 then Exit(Arr[0]);
  Def := Func;
  Result := Def(Arr[0],Arr[1]);
  for i:=2 to High(Arr) do
    Result := Def(Result,Arr[i]);
end;


//---| TPA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TPointArray): TPoint; overload;
var
  i,l:Int32; Def:__TReduceTPA;
begin
  l := High(Arr);
  if l < 0 then Exit(Point(0,0));
  if l = 0 then Exit(Arr[0]);
  Def := Func;
  Result := Def(Arr[0],Arr[1]);
  for i:=2 to High(Arr) do
    Result := Def(Result,Arr[i]);
end;


//---| TBA |---\\
function SimbaExt.Reduce(Func:Pointer; Arr:TBoxArray): TBox; overload;
var
  i,l:Int32; Def:__TReduceTBA;
begin
  l := High(Arr);
  if l < 0 then Exit(TBox([0,0,0,0]));
  if l = 0 then Exit(Arr[0]);
  Def := Func;
  Result := Def(Arr[0],Arr[1]);
  for i:=2 to High(Arr) do
    Result := Def(Result,Arr[i]);
end;




{!DOCREF} {
  @method: function se.Filter(Func:Pointer; Arr:<1d array type>): <1d array type>;
  @desc: 
    Returns a array consisting of those items from the array for which `func(item)` is True. 
    The result will always be of the same type as Arr.[br] 
    
    For example, to compute a sequence of numbers not divisible by 2 or 3:
    [code=pascal]
    function F(x:Int32): Boolean;
    begin Result := (x mod 2 <> 0) and (x mod 3 <> 0); end;
    [br]
    WriteLn( se.Filter(@F, [1,2,3,4,5,6,7,8,9,10]) );
    [/code]
    Output: `[1, 5, 7]`
    
    [params]
    [b]Func:[/b] function that takes 1 parameter (same type as array items), and returns a boolean
    [b]Arr:[/b] TByteArray, TIntArray, TFloatArray, TDoubleArray, TExtArray, TPointArray, TBoxArray
    [/params]
}


{$IFNDEF CODEINSIGHT}
type
  __TFilterTBtA = function (x:Byte): Boolean;
  __TFilterTIA  = function (x:Int32): Boolean;
  __TFilterTFA  = function (x:Single): Boolean;
  __TFilterTDA  = function (x:Double): Boolean;
  __TFilterTEA  = function (x:Extended): Boolean;
  __TFilterTPA  = function (x:TPoint): Boolean;
  __TFilterTBA  = function (x:TBox): Boolean;
{$ENDIF}

//---| TBtA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TByteArray): TByteArray; overload;
var
  i,l,j:Int32; Def:__TFilterTBtA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TIA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TIntArray): TIntArray; overload;
var
  i,l,j:Int32; Def:__TFilterTIA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;

  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TFA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TFloatArray): TFloatArray; overload;
var
  i,l,j:Int32; Def:__TFilterTFA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TDA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TDoubleArray): TDoubleArray; overload;
var
  i,l,j:Int32; Def:__TFilterTDA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TEA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TExtArray): TExtArray; overload;
var
  i,l,j:Int32; Def:__TFilterTEA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TPA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TPointArray): TPointArray; overload;
var
  i,l,j:Int32; Def:__TFilterTPA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;


//---| TBA |---\\
function SimbaExt.Filter(Func:Pointer; Arr:TBoxArray): TBoxArray; overload;
var
  i,l,j:Int32; Def:__TFilterTBA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  j := 0;
  for i:=0 to High(Arr) do
    if Def(Arr[i]) then
    begin
      Result[j] := Arr[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;









{!DOCREF} {
  @method: function se.Map(Func:Pointer; Arr:<1d array type>): <1d array type>;
  @desc: 
    Calls `Func(item)` for each of the array's items and returns an array of the return values.
    The result type will always be the same as the input type.[br]
    
    For example, to compute some cubes:
    [code=pascal]
    function F(x:Int32): Int32;
    begin Result := x*x*x; end; 
    [br]
    WriteLn( se.Map(@F, [1,2,3,4,5,6,7,8,9,10]) );
    [/code]
    Output: `[1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]`
    
    [params]
    [b]Func:[/b] function that takes 1 parameter (same type as array items), and returns a the same type as the array items.
    [b]Arr:[/b] TByteArray, TIntArray, TFloatArray, TDoubleArray, TExtArray, TPointArray, TBoxArray
    [/params]
}

{$IFNDEF CODEINSIGHT}
type
  __TMapTBtA = function (x:Byte): Int32;
  __TMapTIA  = function (x:Int32): Int32;
  __TMapTFA  = function (x:Single): Extended;
  __TMapTDA  = function (x:Double): Extended;
  __TMapTEA  = function (x:Extended): Extended;
  __TMapTPA  = function (x:TPoint): TPoint;
  __TMapTBA  = function (x:TBox): TBox;
{$ENDIF}

//---| TBtA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TByteArray): TByteArray; overload;
var
  i,l:Int32; Def:__TMapTBtA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TIA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TIntArray): TIntArray; overload;
var
  i,l:Int32; Def:__TMapTIA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TFA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TFloatArray): TFloatArray; overload;
var
  i,l:Int32; Def:__TMapTFA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TDA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TDoubleArray): TDoubleArray; overload;
var
  i,l:Int32; Def:__TMapTDA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TEA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TExtArray): TExtArray; overload;
var
  i,l:Int32; Def:__TMapTEA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TPA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TPointArray): TPointArray; overload;
var
  i,l:Int32; Def:__TMapTPA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;


//---| TBA |---\\
function SimbaExt.Map(Func:Pointer; Arr:TBoxArray): TBoxArray; overload;
var
  i,l:Int32; Def:__TMapTBA;
begin
  l := High(Arr);
  if l < 0 then Exit();
  Def := Func;
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := Def(Arr[i]);
end;









{!DOCREF} {
  @method: function se.Range(lo,hi:Int32; step:Int32=1): TIntArray;
  @desc: 
    Generates an array ranging from `lo` to `hi`, with the given `step`.
    Negative `step` will result in a reversed result.
    Alternative methods to return other arraytypes: `RangeB` | `RangeF` | `RangeD` | `RangeE`[br]
    
    Examples:
    [code=pascal]WriteLn( se.Range(0,10) );[/code] Output: `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`[br]
    
    [code=pascal]WriteLn( se.Range(-100,0,-20) );[/code] Output: `[0, -20, -40, -60, -80, -100]`
}
function SimbaExt.Range(lo,hi:Int32; step:Int32=1): TIntArray;
var i,j:Int32;
begin
  j := -1;
  case (step > 0) and True of
    True:
      begin
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=lo to hi with step do
          Result[Inc(j)] := i;
      end;
    False:
      begin
        step := abs(step);
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=hi downto lo with step do
          Result[Inc(j)] := i;
      end;
  end;
end;


function SimbaExt.RangeB(lo,hi:Int32; step:Int32=1): TByteArray;
var i,j:Int32;
begin
  j := -1;
  case (step > 0) and True of
    True:
      begin
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=lo to hi with step do
          Result[Inc(j)] := i;
      end;
    False:
      begin
        step := abs(step);
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=hi downto lo with step do
          Result[Inc(j)] := i;
      end;
  end;
end;


function SimbaExt.RangeF(lo,hi:Int32; step:Int32=1): TFloatArray;
var i,j:Int32;
begin
  j := -1;
  case (step > 0) and True of
    True:
      begin
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=lo to hi with step do
          Result[Inc(j)] := i;
      end;
    False:
      begin
        step := abs(step);
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=hi downto lo with step do
          Result[Inc(j)] := i;
      end;
  end;
end;


function SimbaExt.RangeD(lo,hi:Int32; step:Int32=1): TDoubleArray;
var i,j:Int32;
begin
  j := -1;
  case (step > 0) and True of
    True:
      begin
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=lo to hi with step do
          Result[Inc(j)] := i;
      end;
    False:
      begin
        step := abs(step);
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=hi downto lo with step do
          Result[Inc(j)] := i;
      end;
  end;
end;

function SimbaExt.RangeE(lo,hi:Int32; step:Int32=1): TExtArray;
var i,j:Int32;
begin
  j := -1;
  case (step > 0) and True of
    True:
      begin
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=lo to hi with step do
          Result[Inc(j)] := i;
      end;
    False:
      begin
        step := abs(step);
        SetLength(Result, ((hi-lo) div step) + 1);
        for i:=hi downto lo with step do
          Result[Inc(j)] := i;
      end;
  end;
end;
