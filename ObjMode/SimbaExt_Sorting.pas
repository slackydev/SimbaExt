{!DOCTOPIC}{ 
  Sorting functions
}

{!DOCREF} {
  @method: procedure se.SortTIA(var Arr: TIntArray);  
  @desc: Sorts the TIA from low to high
}
procedure SimbaExt.SortTIA(var Arr: TIntArray);  
begin
  exp_SortTIA(Arr);
end;

{!DOCREF} {
  @method: procedure se.SortTEA(var Arr: TExtArray);  
  @desc: Sorts the TIA from low to high
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
