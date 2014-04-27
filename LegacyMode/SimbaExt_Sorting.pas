{*=========================================================================================|
| Sorting.pas                                                                              |
|=========================================================================================*}
procedure se_SortTIA(var Arr: TIntegerArray);  
begin
  exp_SortTIA(Arr);
end;

procedure se_SortTEA(var Arr: TExtendedArray);  
begin
  exp_SortTEA(Arr);
end;

procedure se_SortTPA(var Arr: TPointArray);  
begin
  exp_SortTPA(Arr);
end;

procedure se_SortTPAFrom(var Arr: TPointArray; const From:TPoint);  
begin
  exp_SortTPAFrom(Arr, From);
end;

procedure se_SortTPAByRow(var Arr: TPointArray);  
begin
  exp_SortTPAByRow(Arr);
end;

procedure se_SortTPAByColumn(var Arr: TPointArray);  
begin
  exp_SortTPAByColumn(Arr);
end;

procedure se_SortTPAByX(var Arr: TPointArray);  
begin
  exp_SortTPAByX(Arr);
end;

procedure se_SortTPAByY(var Arr: TPointArray);  
begin
  exp_SortTPAByY(Arr);
end;