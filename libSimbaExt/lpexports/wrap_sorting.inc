{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap sorting methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure exp_SortTBA(var se:SExt; var Arr: CoreTypes.TByteArray); Cdecl;
begin
  SortTBA(Arr);
end;

procedure exp_SortTIA(var se:SExt; var Arr: TIntArray); Cdecl;
begin
  SortTIA(Arr);
end;

procedure exp_SortTFA(var se:SExt; var Arr: TFloatArray); Cdecl;
begin
  SortTFA(Arr);
end;

procedure exp_SortTDA(var se:SExt; var Arr: TDoubleArray); Cdecl;
begin
  SortTDA(Arr);
end;

procedure exp_SortTEA(var se:SExt; var Arr: TExtArray); Cdecl;
begin
  SortTEA(Arr);
end;

//--------
procedure exp_SortTPA(var se:SExt; var Arr: TPointArray); Cdecl;
begin
  SortTPA(Arr);
end;

procedure exp_SortTPAFrom(var se:SExt; var Arr: TPointArray; const From:TPoint); Cdecl;
begin
  SortTPAFrom(Arr, From);
end;

procedure exp_SortTPAByRow(var se:SExt; var Arr: TPointArray); Cdecl;
begin
  SortTPAByRow(Arr);
end;

procedure exp_SortTPAByColumn(var se:SExt; var Arr: TPointArray); Cdecl;
begin
  SortTPAByColumn(Arr);
end;

procedure exp_SortTPAByX(var se:SExt; var Arr: TPointArray); Cdecl;
begin
  SortTPAByX(Arr);
end;

procedure exp_SortTPAByY(var se:SExt; var Arr: TPointArray); Cdecl;
begin
  SortTPAByY(Arr);
end;

//--------
procedure exp_SortTSA(var se:SExt; var Arr: TStringArray; CaseInsensitve:LongBool=False); Cdecl;
begin
  SortTSA(Arr, CaseInsensitve);
end;

procedure exp_SortTSANatural(var se:SExt; var Arr: TStringArray); Cdecl;
begin
  SortTSANatural(Arr);
end;

//--------
procedure exp_SortATPAByLength(var se:SExt; var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByLength(Arr);
end;

procedure exp_SortATPAByMean(var se:SExt; var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByMean(Arr);
end;

procedure exp_SortATPAByFirst(var se:SExt; var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByFirst(Arr);
end;

procedure exp_SortATPAByIndex(var se:SExt; var Arr:T2DPointArray; index:Int32); Cdecl;
begin
  SortATPAByIndex(Arr, index);
end;

procedure exp_SortATPAFrom(var se:SExt; var Arr:T2DPointArray; point:TPoint); Cdecl;
begin
  SortATPAFrom(Arr, point);
end;

//--------
procedure exp_SortATBAByLength(var se:SExt; var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByLength(Arr);
end;

procedure exp_SortATBAByMean(var se:SExt; var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByMean(Arr);
end;

procedure exp_SortATBAByFirst(var se:SExt; var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByFirst(Arr);
end;

procedure exp_SortATBAByIndex(var se:SExt; var Arr:T2DByteArray; index:Int32); Cdecl;
begin
  SortATBAByIndex(Arr, index);
end;

//--------
procedure exp_SortATIAByLength(var se:SExt; var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByLength(Arr);
end;

procedure exp_SortATIAByMean(var se:SExt; var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByMean(Arr);
end;

procedure exp_SortATIAByFirst(var se:SExt; var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByFirst(Arr);
end;

procedure exp_SortATIAByIndex(var se:SExt; var Arr:T2DIntArray; index:Int32); Cdecl;
begin
  SortATIAByIndex(Arr, index);
end;

//--------
procedure exp_SortATEAByLength(var se:SExt; var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByLength(Arr);
end;

procedure exp_SortATEAByMean(var se:SExt; var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByMean(Arr);
end;

procedure exp_SortATEAByFirst(var se:SExt; var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByFirst(Arr);
end;

procedure exp_SortATEAByIndex(var se:SExt; var Arr:T2DExtArray; index:Int32); Cdecl;
begin
  SortATEAByIndex(Arr, index);
end;