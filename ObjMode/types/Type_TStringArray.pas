(*=============================================================================|
 TStringArray functionality
|=============================================================================*)
{#DOCUMENT} {
  [method]function TStringArray.Clone(): TStringArray;[/method]
  [desc]Returns a copy of the array[desc]
}{#END}
function TStringArray.Clone(): TStringArray;
var
  i:Int32;
begin
  SetLength(Result, Length(Self));
  for i:=0 to High(Self) do
    Result[i] := Copy(Self[i]);
end;


{#DOCUMENT} {
  [method]procedure TStrngArray.Append(const Str:String);[/method]
  [desc]Add another string to the array[/desc]
}{#END}
procedure TStringArray.Append(const Str:String);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Str;
end;


{#DOCUMENT} {
  [method]function TStringArray.Pop(): String;[/method]
  [desc]Removes and returns the last item in the array[/desc]
}{#END}
function TStringArray.Pop(): String;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{#DOCUMENT} {
  [method]function TStringArray.Slice(Start, Stop: Int32): TStringArray;[/method]
  [desc]Returns a slice of the array[/desc]
}{#END}
function TStringArray.Slice(Start,Stop: Int32): TStringArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
end;


{#DOCUMENT} {
  [method]procedure TStringArray.Sort();[/method]
  [desc]
  Sorts the array 
  [note]Partial, key not supported yet[/note]    
  [/desc]
}{#END}
procedure TStringArray.Sort();
begin
  se.SortTSA(Self);
end;


{#DOCUMENT} {
  [method]function TStringArray.Sorted(): TStringArray;[/method]
  [desc] 
    Sorts and returns a copy of the array.
    [note]Partial, key not supported yet[/note]    
  [/desc]
}{#END}
function TStringArray.Sorted(): TStringArray;
begin
  Result := Self.Clone();
  se.SortTSA(Result);
end;



{#DOCUMENT} {
  [method]function TStringArray.Reversed(): TStringArray;[/method]
  [desc] 
    Creates a reversed copy of the array
  [/desc]
}{#END}
function TStringArray.Reversed(): TStringArray;
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{#DOCUMENT} {
  [method]procedure TStringArray.Reverse();[/method]
  [desc] 
    Reverses the array  
  [/desc]
}{#END}
procedure TStringArray.Reverse();
var
  i, Hi, Mid: Integer;
  tmp:String;
begin
  Hi := High(Self);
  if (Hi < 0) then Exit;
  Mid := Hi div 2;
  for i := 0 to Mid do begin
    tmp := Self[Hi-i];
    Self[Hi-i] := Self[i];
    Self[i] := tmp;
  end;
end;
