{*=========================================================================================|
| Numeric.pas                                                                              |
|=========================================================================================*}
function XT_SumTIA(const Arr: TIntegerArray): Integer;  
begin
  Result := exp_SumTIA(Arr);
end;

function XT_SumTEA(const Arr: TExtendedArray): Extended;  
begin
  Result := exp_SumTEA(Arr);
end;

function XT_TIACombinations(const Arr: TIntegerArray; Seq:Integer):  T2DIntegerArray;  
begin
  exp_TIACombinations(Arr, Seq, Result);
end;

function XT_TEACombinations(const Arr: TExtendedArray; Seq:Integer):  T2DExtendedArray;  
begin
  exp_TEACombinations(Arr, Seq, Result);
end;

procedure XT_MinMaxTIA(const Arr: TIntegerArray; var Min:Integer; var Max: Integer);  
begin
  exp_MinMaxTIA(Arr, Min,Max);
end;

procedure XT_MinMaxTEA(const Arr: TExtendedArray; var Min:Extended; var Max: Extended);  
begin
  exp_MinMaxTEA(Arr, Min,Max);
end;

//function XT_TIAMatches(const Arr1, Arr2:TIntegerArray; InPercent, Inversed:Boolean): Integer;  
//begin
//  Result := exp_TIAMatches(Arr1,Arr2, InPercent, Inversed);
//end;

//function XT_LogscaleTIA(const Freq:TIntegerArray; Scale: Integer): TIntegerArray;  
//begin
//  exp_LogscaleTIA(Freq, Scale, Result);
//end;