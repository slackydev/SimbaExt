{*=========================================================================================|
| Sorting.pas                                                                              |
|=========================================================================================*}
procedure SimbaExt.SortTIA(var Arr: TIntegerArray);  
begin
  exp_SortTIA(Arr);
end;

procedure SimbaExt.SortTEA(var Arr: TExtendedArray);  
begin
  exp_SortTEA(Arr);
end;

procedure SimbaExt.SortTPA(var Arr: TPointArray);  
begin
  exp_SortTPA(Arr);
end;

procedure SimbaExt.SortTPAFrom(var Arr: TPointArray; const From:TPoint);  
begin
  exp_SortTPAFrom(Arr, From);
end;

procedure SimbaExt.SortTPAByRow(var Arr: TPointArray);  
begin
  exp_SortTPAByRow(Arr);
end;

procedure SimbaExt.SortTPAByColumn(var Arr: TPointArray);  
begin
  exp_SortTPAByColumn(Arr);
end;

procedure SimbaExt.SortTPAByX(var Arr: TPointArray);  
begin
  exp_SortTPAByX(Arr);
end;

procedure SimbaExt.SortTPAByY(var Arr: TPointArray);  
begin
  exp_SortTPAByY(Arr);
end;