{*=========================================================================================|
| Numeric.pas                                                                              |
|=========================================================================================*}
function SimbaExt.SumTIA(const Arr: TIntegerArray): Integer;  
begin
  Result := exp_SumTIA(Arr);
end;

function SimbaExt.SumTEA(const Arr: TExtendedArray): Extended;  
begin
  Result := exp_SumTEA(Arr);
end;

function SimbaExt.TIACombinations(const Arr: TIntegerArray; Seq:Integer):  T2DIntegerArray;  
begin
  exp_TIACombinations(Arr, Seq, Result);
end;

function SimbaExt.TEACombinations(const Arr: TExtendedArray; Seq:Integer):  T2DExtendedArray;  
begin
  exp_TEACombinations(Arr, Seq, Result);
end;

procedure SimbaExt.MinMaxTIA(const Arr: TIntegerArray; var Min:Integer; var Max: Integer);  
begin
  exp_MinMaxTIA(Arr, Min,Max);
end;

procedure SimbaExt.MinMaxTEA(const Arr: TExtendedArray; var Min:Extended; var Max: Extended);  
begin
  exp_MinMaxTEA(Arr, Min,Max);
end;

//function SimbaExt.TIAMatches(const Arr1, Arr2:TIntegerArray; InPercent, Inversed:Boolean): Integer;  
//begin
//  Result := exp_TIAMatches(Arr1,Arr2, InPercent, Inversed);
//end;

//function SimbaExt.LogscaleTIA(const Freq:TIntegerArray; Scale: Integer): TIntegerArray;  
//begin
//  exp_LogscaleTIA(Freq, Scale, Result);
//end;