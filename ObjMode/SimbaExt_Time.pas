{!DOCTOPIC}{ TimeUtils module }

{!DOCREF} {
  @method: var TimeUtils = TObjTime;
  @desc: Miscellaneous time related functions
}


{!DOCREF} {
  @method: function TimeUtils.Time(Offset:Double=0): Double;
  @desc: 
    Return the time in seconds since the unix-epoch, as double.
    [code=pascal]...[/code]
  
  @keywords: 
    TimeUtils, TimeUtils.Time, Time
}
function TObjTime.Time(Offset:UInt32=0): Double;
begin
  Result := exp_TimeSinceEpoch(Offset);
end;


{!DOCREF} {
  @method: function TimeUtils.Format(FormatString:String; UnixTime:Double): String;
  @desc: 
    Return the time as a formatted string
    [code=pascal]...[/code]
  
  @keywords: 
    TimeUtils, TimeUtils.Format, Format
}
function TObjTime.Format(FormatString:String; UnixTime:Double): String;
begin
  Result := exp_FormatEpochTime(FormatString, UnixTime);
end;


{!DOCREF} {
  @method: function TimeUtils.ToDateTime(UnixTime:Double): TDateTime;
  @desc: 
    Converts Unix time in to TDateTime represntation.
    [code=pascal]...[/code]
  
  @keywords: 
    TimeUtils, TimeUtils.EpochToDateTime, EpochToDateTime
}
function TObjTime.ToDateTime(UnixTime:Double): TDateTime;
begin
  Result := exp_EpochToTime(UnixTime);
end;


{!DOCREF} {
  @method: procedure TimeUtils.Mark(var Timer:Double);
  @desc: Marks current time, used to with `TimeUtils.Ellapsed()` to get time ellapsed since marked.
}
procedure TObjTime.Mark(var Timer:Double);
begin
  Timer := MarkTime();
end;


{!DOCREF} {
  @method: procedure TimeUtils.Ellapsed(Timer:Double; usec:Boolean=False): Double;
  @desc: Returns the time in `ms` since `TimeUtils.Mark()` was called, if `usec = true` then it returns μs.
}
function TObjTime.Ellapsed(Timer:Double; usec:Boolean=False): Double;
begin
  case usec of
    False: Result := Round((MarkTime() - Timer), 5);
    True:  Result := Round((MarkTime() - Timer) * 1000, 5);
  end;
end;


{!DOCREF} {
  @method: procedure TimeUtils.Wait(ms:Double);
  @desc: High precision `Wait()` function. Should atleast beat the precition of the internal `Wait()`. 
}
procedure TObjTime.Wait(ms:Double);
begin
  ms := MarkTime() + ms - 0.002;
  while (MarkTime() < ms) do
    Continue;
end;
