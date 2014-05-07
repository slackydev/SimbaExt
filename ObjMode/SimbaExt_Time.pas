{*=========================================================================================|
| Time.pas (UnixTime / EpochTime)                                                          |
|=========================================================================================*}
{!DOCTOPIC}{ 
  TimeUtils module
}

{!DOCREF} {
  @method: TimeUtils - Miscellaneous time functionality
  desc: This module provides you with a few time related functions.
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
