{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*
  Work in prog
  Returns all the unique values.
*)

//========| 1-dimesional (Simple).. |=========================================|

{$define Uniq1DBody :=
  n := Length(Arr);
  SetLength(Table, Trunc(Pow(2, Floor(Logn(2,n)) + 1)));
  c := 0;
  for i:=0 to n-1 do
  begin
    Isset := False;
    hash := Arr[i] and High(Table);
    l := Length(Table[hash]);
    for j:=0 to l-1 do
      if (Table[hash][j] = Arr[i]) then
      begin
        Isset := True;
        Break;
      end;
    if not(Isset) then
    begin
      SetLength(Table[hash], l+1);
      Table[hash][l] := Arr[i];
      Arr[c] := Arr[i];
      Inc(c);
    end;
  end;
  SetLength(Arr, c);
}


function Uniq(const Arr:TByteArray): TByteArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TIntArray): TIntArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TExtArray): TExtArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TFloatArray): TFloatArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TDoubleArray): TDoubleArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TPointArray): TPointArray; overload;
begin Uniq1DBody end;


function Uniq(const Arr:TBoxArray): TBoxArray; overload;
begin Uniq1DBody end;


//Special
function Uniq(const Arr:String): String; overload;
begin
end;