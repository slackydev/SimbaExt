{*=========================================================================================|
| Time.pas (UnixTime / EpochTime)                                                          |
|=========================================================================================*}
{#DOCUMENT} {
  [method]function TimeUtils.Time(Offset:Double=0): Double;[/method]
  [desc]
    Return the time in seconds since the epoch, as double.
    [code=pascal]...[/code]
  [/desc]
  [keywords]TimeUtils, TimeUtils.Time, Time[/keywords]
}{#END}
function TObjTime.Time(Offset:UInt32=0): Double;
begin
  Result := exp_TimeSinceEpoch(Offset);
end;


{#DOCUMENT} {
  [method]function TimeUtils.Format(FormatString:String; UnixTime:Double): String;[/method]
  [desc]
    Return the time as a formatted string
    [code=pascal]...[/code]
  [/desc]
  [keywords]TimeUtils, TimeUtils.Format, Format[/keywords]
}{#END}
function TObjTime.Format(FormatString:String; UnixTime:Double): String;
begin
  Result := exp_FormatEpochTime(FormatString, UnixTime);
end;


{#DOCUMENT} {
  [method]function TimeUtils.ToDateTime(UnixTime:Double): TDateTime;[/method]
  [desc]
    Converts Unix time in to TDateTime represntation.
    [code=pascal]...[/code]
  [/desc]
  [keywords]TimeUtils, TimeUtils.EpochToDateTime, EpochToDateTime[/keywords]
}{#END}
function TObjTime.ToDateTime(UnixTime:Double): TDateTime;
begin
  Result := exp_EpochToTime(UnixTime);
end;
