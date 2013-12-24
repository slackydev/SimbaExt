{*=========================================================================================|
| Sorting.pas                                                                              |
|=========================================================================================*}
procedure XT_SortTIA(var Arr: TIntegerArray);  
begin
  exp_SortTIA(Arr);
end;

procedure XT_SortTEA(var Arr: TExtendedArray);  
begin
  exp_SortTEA(Arr);
end;

procedure XT_SortTPA(var Arr: TPointArray);  
begin
  exp_SortTPA(Arr);
end;

procedure XT_SortTPAFrom(var Arr: TPointArray; const From:TPoint);  
begin
  exp_SortTPAFrom(Arr, From);
end;

procedure XT_SortTPAByRow(var Arr: TPointArray);  
begin
  exp_SortTPAByRow(Arr);
end;

procedure XT_SortTPAByColumn(var Arr: TPointArray);  
begin
  exp_SortTPAByColumn(Arr);
end;