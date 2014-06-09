{!DOCTOPIC}{ 
  Sorting functions
}

{!DOCREF} {
  @method: procedure se.SortTBA(var Arr: TByteArray);  
  @desc: Sorts the array from low to high
}
procedure SimbaExt.SortTBA(var Arr: TByteArray);
begin
  exp_SortTBA(Arr);
end;


{!DOCREF} {
  @method: procedure se.SortTIA(var Arr: TIntArray);  
  @desc: Sorts the array from low to high
}
procedure SimbaExt.SortTIA(var Arr: TIntArray);  
begin
  exp_SortTIA(Arr);
end;


{!DOCREF} {
  @method: procedure se.SortTFA(var Arr: TFloatArray);  
  @desc: Sorts the array from low to high
}
procedure SimbaExt.SortTFA(var Arr: TFloatArray);  
begin
  exp_SortTFA(Arr);
end;


{!DOCREF} {
  @method: procedure se.SortTDA(var Arr: TDoubleArray);  
  @desc: Sorts the array from low to high
}
procedure SimbaExt.SortTDA(var Arr: TDoubleArray);  
begin
  exp_SortTDA(Arr);
end;


{!DOCREF} {
  @method: procedure se.SortTEA(var Arr: TExtArray);  
  @desc: Sorts the array from low to high
}
procedure SimbaExt.SortTEA(var Arr: TExtArray);  
begin
  exp_SortTEA(Arr);
end;


//TPA
{!DOCREF} {
  @method: procedure se.SortTPA(var Arr: TPointArray);
  @desc: Sorts the TPA from low to high defined by the distnace from Point(0,0) / 'Magnitude'
}
procedure SimbaExt.SortTPA(var Arr: TPointArray);  
begin
  exp_SortTPA(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortTPAFrom(var Arr: TPointArray; const From:TPoint);
  @desc: Sorts the TPA from low to high defined by the distnace from the given TPoint 'from'
}
procedure SimbaExt.SortTPAFrom(var Arr: TPointArray; const From:TPoint);  
begin
  exp_SortTPAFrom(Arr, From);
end;

{!DOCREF} {
  @method: procedure se.SortTPAByRow(var Arr: TPointArray); 
  @desc: Sorts the TPA by Row.
}
procedure SimbaExt.SortTPAByRow(var Arr: TPointArray);  
begin
  exp_SortTPAByRow(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortTPAByColumn(var Arr: TPointArray); 
  @desc: Sorts the TPA by Column.
}
procedure SimbaExt.SortTPAByColumn(var Arr: TPointArray);  
begin
  exp_SortTPAByColumn(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortTPAByX(var Arr: TPointArray); 
  @desc: Sorts the TPA from low to high by each points X-position.
}
procedure SimbaExt.SortTPAByX(var Arr: TPointArray);  
begin
  exp_SortTPAByX(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortTPAByY(var Arr: TPointArray); 
  @desc: Sorts the TPA from low to high by each points Y-position.
}
procedure SimbaExt.SortTPAByY(var Arr: TPointArray);  
begin
  exp_SortTPAByY(Arr);
end;


//TSA
{!DOCREF} {
  @method: procedure se.SortTSA(var Arr: TStringArray; IgnoreCase:Boolean=False);
  @desc: Sorts an array of strings lexicographically, in other words it will ber sorted by it's ASCII value.
  
  If ignoreCase is True then it will igonere if the strings contain lower or upper case when compared
}
procedure SimbaExt.SortTSA(var Arr: TStringArray; IgnoreCase:Boolean=False);  
begin
  exp_SortTSA(Arr,IgnoreCase);
end;

{!DOCREF} {
  @method: procedure se.SortTSANatural(var Arr: TStringArray);
  @desc: Sorts an array of strings in a more logical manner. Sort lexically (case insesitive), but will sort numeral parts numerically
}
procedure SimbaExt.SortTSANatural(var Arr: TStringArray);  
begin
  exp_SortTSANatural(Arr);
end;


//ATPA
{!DOCREF} {
  @method: procedure se.SortATPAByLength(var Arr: T2dPointArray);  
  @desc: Sorts an 'Array of TPointArray' from low to high by array length
}
procedure SimbaExt.SortATPAByLength(var Arr: T2dPointArray);  
begin
  exp_SortATPAByLength(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATPAByMean(var Arr: T2DPointArray);
  @desc: Sorts an 'Array of TPointArray' from low to high by array mean
}
procedure SimbaExt.SortATPAByMean(var Arr: T2DPointArray);  
begin
  exp_SortATPAByMean(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATPAByFirst(var Arr: T2DPointArray);
  @desc: Sorts an 'Array of TPointArray' from low to high by arrays first item
}
procedure SimbaExt.SortATPAByFirst(var Arr: T2DPointArray);  
begin
  exp_SortATPAByFirst(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATPAByIndex(var Arr: T2DPointArray; Index: Int32);
  @desc: Sorts an 'Array of TPointArray' from low to high by the selected array item
}
procedure SimbaExt.SortATPAByIndex(var Arr: T2DPointArray; Index: Int32);  
begin
  exp_SortATPAByIndex(Arr, Index);
end;




//ATBA
{!DOCREF} {
  @method: procedure se.SortATBAByLength(var Arr: T2DByteArray);  
  @desc: Sorts an 'Array of TByteArray' from low to high by array length
}
procedure SimbaExt.SortATBAByLength(var Arr: T2DByteArray);  
begin
  exp_SortATBAByLength(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATIAByMean(var Arr: T2DByteArray);  
  @desc: Sorts an 'Array of TByteArray' from low to high by array mean
}
procedure SimbaExt.SortATBAByMean(var Arr: T2DByteArray);  
begin
  exp_SortATBAByMean(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATBAByFirst(var Arr: T2DByteArray);
  @desc: Sorts an 'Array of TByteArray' from low to high by arrays first item
}
procedure SimbaExt.SortATBAByFirst(var Arr: T2DByteArray);  
begin
  exp_SortATBAByFirst(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATBAByIndex(var Arr: T2DByteArray; Index: Int32);
  @desc: Sorts an 'Array of TByteArray' from low to high by the selected array item
}
procedure SimbaExt.SortATBAByIndex(var Arr: T2DByteArray; Index: Int32);  
begin
  exp_SortATBAByIndex(Arr, Index);
end;





//ATIA
{!DOCREF} {
  @method: procedure se.SortATIAByLength(var Arr: T2DIntArray);  
  @desc: Sorts an 'Array of TIntArray' from low to high by array length
}
procedure SimbaExt.SortATIAByLength(var Arr: T2DIntArray);  
begin
  exp_SortATIAByLength(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATIAByMean(var Arr: T2DIntArray);  
  @desc: Sorts an 'Array of TIntArray' from low to high by array mean
}
procedure SimbaExt.SortATIAByMean(var Arr: T2DIntArray);  
begin
  exp_SortATIAByMean(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATIAByFirst(var Arr: T2DIntArray);
  @desc: Sorts an 'Array of TIntArray' from low to high by arrays first item
}
procedure SimbaExt.SortATIAByFirst(var Arr: T2DIntArray);  
begin
  exp_SortATIAByFirst(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATIAByIndex(var Arr: T2DIntArray; Index: Int32);
  @desc: Sorts an 'Array of TIntArray' from low to high by the selected array item
}
procedure SimbaExt.SortATIAByIndex(var Arr: T2DIntArray; Index: Int32);  
begin
  exp_SortATIAByIndex(Arr, Index);
end;



//ATEA
{!DOCREF} {
  @method: procedure se.SortATEAByLength(var Arr: T2DExtArray);  
  @desc: Sorts an 'Array of TExtArray' from low to high by array length
}
procedure SimbaExt.SortATEAByLength(var Arr: T2DExtArray);  
begin
  exp_SortATEAByLength(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATEAByMean(var Arr: T2DExtArray);  
  @desc: Sorts an 'Array of TExtArray' from low to high by array mean
}
procedure SimbaExt.SortATEAByMean(var Arr: T2DExtArray);  
begin
  exp_SortATEAByMean(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATEAByFirst(var Arr: T2DExtArray);
  @desc: Sorts an 'Array of TExtArray' from low to high by arrays first item
}
procedure SimbaExt.SortATEAByFirst(var Arr: T2DExtArray);  
begin
  exp_SortATEAByFirst(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortATIAByIndex(var Arr: T2DExtArray; Index: Int32);
  @desc: Sorts an 'Array of TExtArray' from low to high by the selected array item
}
procedure SimbaExt.SortATEAByIndex(var Arr: T2DExtArray; Index: Int32);  
begin
  exp_SortATEAByIndex(Arr, Index);
end;

