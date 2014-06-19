{!DOCTOPIC}{ HashMap }

type
  TEntryVariant = record Key, Value: Variant; end;
  THashMap = record
  //private
    FTable: Array of Array of TEntryVariant;
    FLen: Integer;
  //public
    //constructor Create(Size:Integer);
    //function Get(Key: UInt32; var Value: Int32): Boolean; Inline;
    //function Add(Key: UInt32; Value:Int32): Boolean;      Inline;
    //Destructor Destroy; override;
  end;

{!DOCREF} {
  @method: procedure THashMap.Create(Capacity:Int32=768);
  @desc: Creates a new hashmap of the given size. `Capacity` should always be a bit larger then the actuall length for optimal performance.
}
procedure THashMap.Create(Capacity:Int32=768);
begin
  FLen := Math.NextPow2m1(Trunc(Capacity * 1.25));
  SetLength(FTable, FLen+1);
end;


{!DOCREF} {
  @method: procedure THashMap.Free();
  @desc: Frees the hashmap.
}
procedure THashMap.Free();
begin
  SetLength(Self.FTable, 0);
  Self.FLen := 0;
end;


{!DOCREF} {
  @method: procedure THashMap.FromString(DStr:String; Sep:Char=';'; Capacity:Int32=12);
  @desc: 
    Converts the string in to a dictionary/hashmap.
    [code=pascal]
      var
        h:THashMap;
        Str:String; x:Double;
      begin
        h.FromString('x:1.9; 6.5:hello world');
        h.Get('x', x);
        h.Get(6.5, str);

        WriteLn(x);
        WriteLn(str);
      end.
    [/code]
}
procedure THashMap.FromString(DStr:String; Sep:Char=';'; Capacity:Int32=12);
var
  v: String;
  i:Int32;
  Items,kv: TStringArray;
begin
  if Sep = ':' then RaiseException('THashMap.FromString: Separator cannot be colon');
  Items := DStr.Split(Sep);
  Self.Create(Max(Length(Items)*3, Capacity));
  for i:=0 to High(Items) do
  begin
    kv := Items[i].Split(':');
    if length(kv) <> 2 then
      RaiseException('THashMap.FromString: Item: "'+Items[i]+'" is invalid');
    V := kv[0].strip(' ');
    if V.IsFloat() then
      Self.Add(VarAsType(v,varDouble),kv[1].strip(' '))
    else if V.IsDigit() then
      if length(V) > 7 then begin
        Self.Add(VarAsType(v,varInt64),kv[1].strip(' '))
      end else begin
        Self.Add(VarAsType(v,varInteger),kv[1].strip(' '));
      end
    else
      Self.Add(v,kv[1].strip(' '));
  end;
end;



function THashMap.Hash(x:Variant): UInt32;
var s:String; ptr:PChar; hiptr:UInt32; tmp:Double;
begin
  if VarIsNumeric(x) and Not(VarIsFloat(x)) then Exit(x and FLen);
  Result := 5381;
  s := VarAsType(x,varString);
  WriteLn(VarAsType(x,varString));
  ptr := PChar(s);
  hiptr := UInt32(ptr[0]+Length(S));
  while (UInt32(ptr) < hiptr) do begin
    Result := ((Result shl 5) + Result) + ord(ptr^);
    inc(ptr);
  end;
  Result := Result and FLen;
end;

function CompareVarKeys(v1,v2:Variant): Boolean;
begin
  if VarIsStr(v1)     and Not(VarIsStr(v2))     then Exit(False);
  if VarIsFloat(v1)   and Not(VarIsFloat(v2))   then Exit(False);
  if VarIsOrdinal(v1) and Not(VarIsOrdinal(v2)) then Exit(False);
  if VarIsNumeric(v1) and Not(VarIsNumeric(v2)) then Exit(False);
  Result := v1 = v2;
end;


{!DOCREF} {
  @method: function THashMap.Add(Key: Variant; Value:Variant): Boolean;
  @desc: 
    Adds a new item to the hashmap.
    
    [b]Keys supported:[/b] Digits, Strings, Floats and Boolean
    [b]Values supported:[/b] Digits, Strings, Floats, Boolean, TPoint, TBox
    
    [code=pascal]
    var
      h:THashMap;
      str:String;
    begin
      h.Create();
      h.Add(5,'hello world');
      h.Get(5,str);
      WriteLn(str);
    end.  
    [/code]
    >> `hello world`
}
function THashMap.Add(Key: Int32; Value:Int32): Boolean;
var h,l,i: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Key and FLen;
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Self.FTable[h][i].Value := Value;
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;

function THashMap.Add(Key: Variant; Value:TPoint): Boolean; overload;
var h,l,i: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Self.FTable[h][i].Value := ToString(Value);
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := ToString(Value);
  Result := True;
end;


function THashMap.Add(Key: Variant; Value:TBox): Boolean; overload;
var h,l,i: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Self.FTable[h][i].Value := ToString(Value);
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := ToString(Value);
  Result := True;
end;


function THashMap.Add(Key: Variant; Value:Variant): Boolean; overload;
var h,l,i: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Self.FTable[h][i].Value := Value;
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;


{!DOCREF} {
  @method: function THashMap.Get(Key: Variant; var Value:Variant): Boolean;
  @desc: 
    Gets the given item `Key` from the hashmap.. and puts the value that belngs to the `Key` in to the result `Value`. Returns False if the item was not found.[br]
    
    [b]Keys supported:[/b] Digits, Strings, Floats and Boolean
    [b]Values supported:[/b] Digits, Strings, Floats, Boolean, TPoint, TBox
}
function THashMap.Get(Key: Variant; var Value: Int32): Boolean;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;


function THashMap.Get(Key: Variant; var Value: Double): Boolean;  overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;


function THashMap.Get(Key: Variant; var Value: Extended): Boolean;  overload;
var
  h,i,l: Int32;
begin
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;


function THashMap.Get(Key: Variant; var Value: String): Boolean;  overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;


function THashMap.Get(Key: Variant; var Value: TPoint): Boolean;  overload;
var
  h,i,j,l: Int32;
  tmp,x,y:String;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      tmp := Self.FTable[h][i].Value;
      if tmp.startswith('{X = ') then
      begin
        j:=6;
        while tmp[j].IsDigit() do begin x := x+tmp[j]; inc(j); end;
        inc(j,6);
        while tmp[j].IsDigit() do begin y := y+tmp[j]; inc(j); end;
      end else
        RaiseException('Item "'+tmp+'" is not a valid TPoint');
      Value := [StrToInt(x), StrToInt(y)];
      Exit(True);
    end;
  Result := False;
end;


function THashMap.Get(Key: Variant; var Value: TBox): Boolean;  overload;
var
  h,i,j,l: Int32;
  tmp,x1,y1,x2,y2:String;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      tmp := Self.FTable[h][i].Value;
      if tmp.startswith('{X1 = ') then
      begin
        j:=7;
        while tmp[j].IsDigit() do begin x1 := x1+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin y1 := y1+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin x2 := x2+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin y2 := y2+tmp[j]; inc(j); end;
      end else
        RaiseException('Item "'+tmp+'" is not a valid TBox');
      Value := [StrToInt(x1), StrToInt(y1), StrToInt(x2), StrToInt(y2)];
      Exit(True);
    end;
  Result := False;
end;

function THashMap.Get(Key: Variant; var Value: Variant): Boolean; overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;


{!DOCREF} {
  @method: function THashMap.Get(Key: Variant): Variant; overload;
  @desc: 
    Gets the given item `Key` from the hashmap.. and puts the value that belngs to the `Key` in to the result `Value`. Returns False if the item was not found.[br]
    
    [b]Keys supported:[/b] Digits, Strings, Floats and Boolean
    [b]Values supported:[/b] Digits, Strings, Floats and Boolean[br]

    Related to support returning `TPoint` and `TBox`:
    `function THashMap.GetPt(Key: Variant): TPoint;`
    `function THashMap.GetBox(Key: Variant): TBox;`[br]
    
    [note]Raises exception if not found![/note]
}
function THashMap.Get(Key: Variant): Variant; overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
      Exit(Self.FTable[h][i].Value);
  RaiseException('THashMap: Key "'+Key+'" was not found');
end;

function THashMap.Get(Key: Int32): Variant; overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
      Exit(Self.FTable[h][i].Value);

  RaiseException(String('THashMap: Key '+ToString(Key)+' was not found'));
end;


function THashMap.Get(Key: Double): Variant; overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
      Exit(Self.FTable[h][i].Value);

  RaiseException(String('THashMap: Key '+ToString(Key)+' was not found'));
end;


function THashMap.Get(Key: String): Variant; overload;
var
  h,i,l: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
      Exit(Self.FTable[h][i].Value);

  RaiseException(String('THashMap: Key '+ToString(Key)+' was not found'));
end;

function THashMap.GetPt(Key: Variant): TPoint; overload;
var
  h,i,l,j: Int32; x,y,tmp:String;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      tmp := Self.FTable[h][i].Value;
      if tmp.startswith('{X = ') then
      begin
        j:=6;
        while tmp[j].IsDigit() do begin x := x+tmp[j]; inc(j); end;
        inc(j,6);
        while tmp[j].IsDigit() do begin y := y+tmp[j]; inc(j); end;
      end else
        RaiseException('Item "'+tmp+'" is not a valid TPoint');
      Exit(Point(StrToInt(x), StrToInt(y)));
    end;
  RaiseException('THashMap: Key "'+Key+'" was not found');
end;


function THashMap.GetBox(Key: Variant): TBox; overload;
var
  h,i,l,j: Int32; x1,y1,x2,y2,tmp:String;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      tmp := Self.FTable[h][i].Value;
      if tmp.startswith('{X1 = ') then
      begin
        j:=7;
        while tmp[j].IsDigit() do begin x1 := x1+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin y1 := y1+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin x2 := x2+tmp[j]; inc(j); end;
        inc(j,7);
        while tmp[j].IsDigit() do begin y2 := y2+tmp[j]; inc(j); end;
      end else
        RaiseException('Item "'+tmp+'" is not a valid TBox');
      Exit(ToBox(StrToInt(x1), StrToInt(y1), StrToInt(x2), StrToInt(y2)));
    end;
  RaiseException('THashMap: Key "'+Key+'" was not found');
end;


{!DOCREF} {
  @method: function THashMap.Remove(Key: Variant): Boolean;
  @desc: 
    Removes the item belonging to the given `Key` from the hashmap. Reutrns `False` if it does not exist.
}
function THashMap.Remove(Key: Variant): Boolean;
var
  h,i,l,j: Int32;
begin
  if FLen = 0 then RaiseException('THashMap: Not yet initalized');
  h := Self.Hash(Key);
  l := High(Self.FTable[h]);
  for i:=0 to l do
    if CompareVarKeys(self.FTable[h][i].Key, Key)  then
    begin
      for j:=i+1 to l do
        Self.FTable[h][j-1] := Self.FTable[h][j];
      SetLength(Self.FTable[h], l);
      Exit(True);
    end;
  Result := False;
end;



{!DOCREF} {
  @method: function THashMap.ToString(Sep:String='; '): String;
  @desc: 
    Converts the hashmap in to a readable string. The string should be compatible with "FromString", as long as you remove the beginning and ending "bracket".
}
function THashMap.ToString(Sep:String='; '): String;
var
  i,j:Int32;
begin
  Result := '{';
  for i:=0 to High(FTable) do
  begin
    for j:=0 to High(FTable[i]) do
    begin
      if Result = '{' then
        Result := '{'+ VarAsType(FTable[i][j].key, varString) +':'+ VarAsType(FTable[i][j].value, varString)
      else
        Result := Result + sep + VarAsType(FTable[i][j].key, varString) +':'+ VarAsType(FTable[i][j].value, varString);

    end;
  end;
  Result := Result + '}';
end;
