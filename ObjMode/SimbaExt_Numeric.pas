{!DOCTOPIC}{ 
  Numeric functions
}

{!DOCREF} {
  @method: function se.SumTBA(const Arr: TByteArray): Int64;  
  @desc: Returns the total sum of the array
}
function SimbaExt.SumTBA(const Arr: TByteArray): Int64;  
begin
  Result := exp_SumTBA(Arr);
end;


{!DOCREF} {
  @method: function se.SumTIA(const Arr: TIntArray): Int64;  
  @desc: Returns the total sum of the array
}
function SimbaExt.SumTIA(const Arr: TIntArray): Int64;  
begin
  Result := exp_SumTIA(Arr);
end;


{!DOCREF} {
  @method: function se.SumTEA(const Arr: TExtArray): Extended; 
  @desc: Returns the total sum of the array
}
function SimbaExt.SumTEA(const Arr: TExtArray): Extended;  
begin
  Result := exp_SumTEA(Arr);
end;


{!DOCREF} {
  @method: function se.TIACombinations(const Arr: TIntArray; Seq:Integer):  T2DIntegerArray;
  @desc: Returns all the possible combinations in the array
}
function SimbaExt.TIACombinations(const Arr: TIntArray; Seq:Integer):  T2DIntegerArray;  
begin
  Result := exp_TIACombinations(Arr, Seq);
end;


{!DOCREF} {
  @method: function se.TEACombinations(const Arr: TExtArray; Seq:Integer):  T2DExtendedArray;
  @desc: Returns all the possible combinations in the array
}
function SimbaExt.TEACombinations(const Arr: TExtArray; Seq:Integer):  T2DExtendedArray;  
begin
  Result := exp_TEACombinations(Arr, Seq);
end;


{!DOCREF} {
  @method: procedure se.MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte);  
  @desc: Returns the lower and upper value in the array
}
procedure SimbaExt.MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte);  
begin
  exp_MinMaxTBA(Arr, Min,Max);
end;


{!DOCREF} {
  @method: procedure se.MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer);  
  @desc: Returns the lower and upper value in the array
}
procedure SimbaExt.MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer);  
begin
  exp_MinMaxTIA(Arr, Min,Max);
end;


{!DOCREF} {
  @method: procedure se.MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended);
  @desc: Returns the lower and upper value in the array
}
procedure SimbaExt.MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended);  
begin
  exp_MinMaxTEA(Arr, Min,Max);
end;

