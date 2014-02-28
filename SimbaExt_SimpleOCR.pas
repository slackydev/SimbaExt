{$include_once TRafikiBitmap.pas}
{*=========================================================================================|
| SimpleOCR.pas                                                                               |
|=========================================================================================*}
function se_ImGetText(ImgArr:T2DIntegerArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString;
begin
  Result := exp_ImGetText(ImgArr, Font, MinCharSpace, MinSpace, TextPixTol, Range);
end;


{*
 ....
*}
function se_LoadFontColor(FontPath:String; Color,Tol:Integer): Array of T2DIntArray; //TChars
var
  path:String;
  TPA:TPointArray;
  i:Integer;
  tmp:TRafBitmap;  
  B:TBox;
begin
  SetLength(Result, 256);
  if not(DirectoryExists(FontPath)) then
  begin
    WriteLn('UNABLE TO LOAD FONT: "' + FontPath + '". Path does not exist.'); 
    Exit;
  end;
  for i:=0 to 255 do
  begin      
    path := FontPath+'\'+IntToStr(ord(i)) +'.bmp'; 
    if FileExists(path) then
    begin
      tmp.Open(path); 
      Result[i] := tmp.ToMatrix();
      se_ImFindColorTolEx(Result[i],TPA,Color,Tol);
      SetLength(Result[i], 0);
      B := GetTPABounds(TPA);
      Result[i] := NewMatrix((B.x2 - B.x1)+1, (B.y2 - B.y1)+1);
      OffsetTPA(TPA, Point(-B.x1,-B.y1));
      Result[i].InsertTPA(TPA, 255);
      tmp.SetSize(1,1);
      tmp.Clear(0);
      tmp.Free();
    end;
  end;
end;


{*
 ....
*}
function se_LoadFontEx(FontPath:String; Invert:Boolean): Array of T2DIntArray; //TChars
var
  path:String;
  TPA:TPointArray;
  i:Integer;
  tmp:TRafBitmap;  
  B:TBox;
begin
  SetLength(Result, 256);
  if not(DirectoryExists(FontPath)) then
  begin
    WriteLn('UNABLE TO LOAD FONT: "' + FontPath + '". Path does not exist.'); 
    Exit;
  end;
  for i:=0 to 255 do
  begin      
    path := FontPath+'\'+IntToStr(ord(i)) +'.bmp'; 
    if FileExists(path) then
    begin
      tmp.Open(path); 
      Result[i] := se_ImThreshold(tmp.ToMatrix(),127,0,255,Invert);
      se_ImFindColorTolEx(Result[i],TPA,255,1);
      SetLength(Result[i], 0);
      B := GetTPABounds(TPA);
      Result[i] := NewMatrix((B.x2 - B.x1)+1, (B.y2 - B.y1)+1);
      OffsetTPA(TPA, Point(-B.x1,-B.y1));
      Result[i].InsertTPA(TPA, 255);
      tmp.SetSize(1,1);
      tmp.Clear(0);
      tmp.Free();
    end;
  end;
end;


{*
 ....
*}
function se_LoadFont(FontPath:String): TChars;
begin
  Result := se_LoadFontEx(FontPath, False);
end;


{*
 ....
*}
function se_GetTextFromTPA(TPA:TPointArray; Width:Integer; Font: TChars; MinCharSpace, MinSpace, PixelNoiseTol, RowSpace:Integer; Range: String): String;
var
  ImgArr:T2DIntArray;
  Rows: T2DPointArray;
  W,H,i,j:Integer;
  RowText: String; 
  B:TBox;
begin
  Rows := se_ClusterTPAEx(TPA, Width, RowSpace, True);
  for i:=0 to High(Rows) do
  begin 
    B := GetTPABounds(Rows[i]);
    W := B.X2 - B.X1 + 1;
    H := B.Y2 - B.Y1 + 1;
    SetLength(ImgArr, 0);  
    ImgArr := NewMatrix(W,H);
    OffsetTPA(TPA, Point(-B.x1,-B.y1));
    ImgArr.InsertTPA(Rows[i], $FFFFFF);
    OffsetTPA(TPA, Point(B.x1,B.y1)); 
    try RowText := se_ImGetText(ImgArr, Font, MinCharSpace,  MinSpace, PixelNoiseTol, Range)
    except Continue;
    end;
    if RowText[1] = ' ' then
    begin
      j := 1;
      while (RowText[j] = ' ') do begin
        inc(j);
        if j = length(RowText) then Break;
      end;
      RowText := Copy(RowText, j, Length(RowText));
    end;
    if (i = 0) then 
      Result := RowText
    else 
      Result := Result + #13#10 + RowText;
  end;
  SetLength(ImgArr, 0);
end;

  
{*
 ....
*}
function se_GetTextAt(Area:TBox; Font:TChars; MinSpace, TextColor: Integer; ColorTol: Integer = 0; PixelNoiseTol:Integer = 0): String; overload;
var
  ImgArr:T2DIntArray;
  TPA: TPointArray;
  bmp: TRafBitmap;
begin
  bmp.FromClient(Area.X1,Area.Y1,Area.X2,Area.Y2);
  ImgArr := Bmp.ToMatrix();
  bmp.Free();
  se_ImFindColorTolEx(ImgArr, TPA, TextColor, ColorTol);
  se_SortTPAByRow(TPA);
  Result := se_GetTextFromTPA(TPA, (Area.x2-Area.x1+1), Font, 1, MinSpace, PixelNoiseTol, 1, '');
end;


function se_GetTextDynAt(Area:TBox; Font:TChars; MinSpace, PixelNoiseTol: Integer; Invert:Boolean=False): String;
var
  ImgArr:T2DIntArray;
  TPA: TPointArray;
  bmp:TRafBitmap;
begin
  bmp.FromClient(Area.X1,Area.Y1,Area.X2,Area.Y2);
  ImgArr := Bmp.ToMatrix();
  bmp.Free();
  ImgArr := se_ImThresholdAdaptive(ImgArr, 0, 255, Invert, tmMean, 50);
  se_ImFindColorTolEx(ImgArr, TPA, 255, 1);
  se_SortTPAByRow(TPA);
  Result := se_GetTextFromTPA(TPA, (Area.x2-Area.x1+1), Font, 1, MinSpace, PixelNoiseTol, 1, '');
end;