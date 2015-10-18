{!DOCTOPIC}{ Standard functions }


{!DOCREF} {
  @method: function se.IndexOf(var Haystack, Needle; ElmntSize:SizeInt): Int32;
  @desc:
    Finds the position of the Needle in the Haystack. Both needle and haystack can be [i]any data-type[/i], tho haystack should always be an Array.
    Requres the size of the elements in the array | Use: `SizeOf(<DataType>)`.
    
    [b]Example:[/b]
    [code=pascal]
    var Arr:TIntArray; Item:Int32;
    begin
      Arr := [0,2,4,6,8,10];
      Item := 8;
      
      WriteLn( se.IndexOf(Arr[0], Item, SizeOf(Integer)) );
    end;  
    [/code]>> `4`
    
    [params]
      [b]Haystack:[/b] Reference to the haystack start.
      [b]Needle:[/b]   The needle.
      [b]ElSize:[/b]   Size of each element.
    [/params]
}
function SimbaExt.IndexOf(var Haystack, Needle; ElSize:SizeInt): Int32;
var
  i,hi,lo,len:Int32;
  Data,Seek,P,Q:PChar;
begin
  Data := PChar(@Haystack);
  Seek := PChar(@Needle);
  Len := PInt32(Data[-4])^ + 1;

  P := Data[0];
  Q := Seek[0];
  lo := Int32(Data[0]);
  hi := Int32(Data[Len*ElSize] - ElSize);
  while hi > UInt32(P) do
  begin
    if (Q^ <> P^) then begin
      inc(p,ElSize);
      continue;
    end;
    if CompareMem(Q, P, ElSize) then
      Exit((UInt32(P)-lo) div ElSize);
    inc(p,ElSize);
  end;
  Exit(-1);
end;

