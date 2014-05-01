{$IFDEF LAPE}
(*=============================================================================|
 Object oriented wrapper for Simba Bitmaps
|=============================================================================*)
type
  TRafBitmap = record
    Bitmap: Integer;
    Width, Height: Integer;
    Loaded:Boolean;
  end;
  


(*=============================================================================|
 Regular intilaization
|=============================================================================*)
procedure TRafBitmap.Create(W, H: Integer; Str:String);
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := BitmapFromString(W,H,Str);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.Create(W,H: Integer); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := CreateBitmap(W,H);
  Self.Loaded := True;
  Self.Width  := W;
  Self.Height := H;
end;


procedure TRafBitmap.Create(SimbaBitmap:Integer); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := SimbaBitmap;
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.Open(ImgPath:String);
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := LoadBitmap(ImgPath);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.FromClient();
var 
  W,H:Integer;
begin
  if Self.Loaded then Self.Free();
  GetClientDimensions(W,H);
  Self.Bitmap := BitmapFromClient(0,0,W-1,H-1); 
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.FromClient(X1,Y1,X2,Y2:Integer); overload;
var 
  W,H:Integer;
begin
  if Self.Loaded then Self.Free();
  GetClientDimensions(W,H);
  
  if (X2 >= W) or (X2 <= -1) then X2 := W-1;
  if (Y2 >= H) or (Y2 <= -1) then Y2 := H-1;
  if (X1 > X2) or (Y1 > Y2) then Exit;
  Self.Bitmap := BitmapFromClient(X1,Y1,X2,Y2); 
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;



(*=============================================================================|
 SCAR Divi compatibility loading
|=============================================================================*)
procedure TRafBitmap.LoadFromJpeg(ImgPath:String);
begin
  TRafBitmap.Open(ImgPath);
end;

procedure TRafBitmap.LoadFromBmp(ImgPath:String);
begin
  TRafBitmap.Open(ImgPath);
end;

procedure TRafBitmap.LoadFromPng(ImgPath:String);
begin
  TRafBitmap.Open(ImgPath);
end;



(*=============================================================================|
 Saving bitmap
|=============================================================================*)
function TRafBitmap.Save(ImgPath:String): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.Save()')) then Exit;
  SaveBitmap(Self.Bitmap, ImgPath);
end;

// SCAR Divi dummy methods:
function TRafBitmap.SaveToJpeg(ImgPath:String): TRafBitmap;
begin
  Self.Save(ImgPath);
end;

function TRafBitmap.SaveToBmp(ImgPath:String): TRafBitmap;
begin
  Self.Save(ImgPath);
end;

function TRafBitmap.SaveToPng(ImgPath:String): TRafBitmap;
begin
  Self.Save(ImgPath);
end;



(*=============================================================================|
 General functinality
|=============================================================================*)
procedure TRafBitmap.SetSize(NewWidth,NewHeight:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.SetSize()')) then Exit;
  SetBitmapSize(Self.Bitmap,NewWidth,NewHeight);
  Self.Width  := NewWidth;
  Self.Height := NewHeight;
end;


procedure TRafBitmap.Clear(Color:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Clear()')) then Exit;
  FastDrawClear(Self.Bitmap, Color);
end;


function TRafBitmap.Clone(): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.Clone()')) then Exit;
  Result.Bitmap := CopyBitmap(Self.Bitmap);
  Result.Width  := Self.Width;
  Result.Height := Self.Height;
  Result.Loaded := True;
end;


procedure TRafBitmap.Crop(X1,Y1,X2,Y2:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Crop()')) then Exit;
  CropBitmap(Self.Bitmap, X1,Y1,X2,Y2);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
end;


function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.GetPixels()')) then Exit;
  Result := FastGetPixels(Self.Bitmap, TPA);
end;


function TRafBitmap.Pixel(x,y:Integer): Integer;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  Result := FastGetPixel(Self.Bitmap, x,y);
end;


procedure TRafBitmap.Pixel(x,y, color:Integer); overload
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  FastSetPixel(Self.Bitmap, x,y, color);
end;


procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Integer);
var
  i,x,y, Hi: Integer;
begin
  if not(Self.IsLoaded('TRafBitmap.SetPixels()')) then Exit;
  Hi := High(TPA);
  if (Hi < -1) then Exit;
  for i := 0 to Hi do
  begin
    x := TPA[i].X;
    y := TPA[i].Y;
    if ((x >= 0) and (y >= 0) and
        (x < Self.Width) and (y < Self.Height)) then
      Self.Pixel(x,y, color);
  end;
end;


procedure TRafBitmap.SetPixelsEx(TPA:TPointArray; Colors:TIntegerArray);
begin
  if not(Self.IsLoaded('TRafBitmap.SetPixelsEx()')) then Exit;
  FastSetPixels(Self.Bitmap, TPA, Colors);
end;


procedure TRafBitmap.ReplaceColor(OldColor, NewColor: Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.ReplaceColor()')) then Exit;
  FastReplaceColor(Self.Bitmap, OldColor, NewColor);
end;


function TRafBitmap.ToMatrix(): T2DIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.ToMatrix()')) then Exit;
  Result := BitmapToMatrix(Self.Bitmap);
end;


procedure TRafBitmap.FromMatrix(Matrix: T2DIntegerArray);
begin
  if Self.Loaded then
  begin
    DrawMatrixBitmap(Self.Bitmap, Matrix);
    GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  end else
  begin
    Self.Create(1,1);
    DrawMatrixBitmap(Self.Bitmap, Matrix);
    GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  end;
end;


function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Tolerance:Integer): Boolean;
var 
  Matrix: T2DIntArray;
begin
  Result := False;
  if not(Self.IsLoaded('TRafBitmap.FindColorTol()')) then Exit;
  
  if (Area.X2 >= Self.Width) then Area.X2 := Self.Width - 1
  else if (Area.X2 <= -1) then Area.X2 := Self.Width - Area.x2;
  
  if (Area.Y2 >= Self.Height) then Area.Y2 := Self.Height - 1
  else if (Area.Y2 <= -1) then Area.Y2 := Self.Height - Area.y2;
  
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;
  
  Matrix := Self.ToMatrix();
  Matrix := Matrix.GetArea(Area.x1,Area.y1,Area.x2,Area.y2);
  Result := se.ImFindColorTolEx(Matrix, TPA, Color, Tolerance);
  SetLength(Matrix, 0);
  if not(Result) then Exit;
  if (Area.X1=0) and (Area.Y1 = 0) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;


function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
begin
  Result := Self.FindColorTol(TPA, Color, Area, 0);
end;


function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
begin
  Result := Self.FindColorTol(TPA, Color, IntToBox(0,0,self.width,self.height), 0);
end;



(*=============================================================================|
 Transformations
|=============================================================================*)
procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Resize()')) then Exit;
  ResizeBitmapEx(Self.Bitmap, RM_Nearest, NewWidth, NewHeight);
  Self.Width := NewWidth;
  Self.Height := NewHeight;
end;


procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:TxResizeMethod);
var
  Matrix:T2DIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.ResizeEx()')) then Exit;
  Matrix := Self.ToMatrix();
  se.ImResize(Matrix, NewWidth, NewHeight, Resampler);
  Self.FromMatrix(Matrix); 
end;

procedure TRafBitmap.Rotate(const Degrees:Extended);
var
  BMP:Integer;
begin
  if not(Self.IsLoaded('TRafBitmap.Rotate()')) then Exit;
  BMP := RotateBitmap(Self.Bitmap, Radians(Degrees));
  FreeBitmap(Self.Bitmap);
  Self.Bitmap := BMP;
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
end;

function TRafBitmap.RotateCopy(Degrees:Extended): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.RotateCopy()')) then Exit;
  Result.Bitmap := RotateBitmap(Self.Bitmap, Radians(Degrees));
  GetBitmapSize(Result.Bitmap, Result.Width, Result.Height);
  Result.Loaded := True;
end;


function TRafBitmap.Flip(Horizontal:Boolean): TRafBitmap;
var 
  Method: TBmpMirrorStyle;
begin
  if not(Self.IsLoaded('TRafBitmap.Flip()')) then Exit;
  case Horizontal of
    True: Method := MirrorWidth;
    False:Method := MirrorHeight;
  end;
  Result.Bitmap := CreateMirroredBitmapEx(Self.Bitmap, Method);
  GetBitmapSize(Result.Bitmap, Result.Width, Result.Height);
  Result.Loaded := True;
end;


procedure TRafBitmap.Invert();
begin
  if not(Self.IsLoaded('TRafBitmap.Invert()')) then Exit;
  InvertBitmap(Self.Bitmap);
end;


procedure TRafBitmap.Blur(BlurSize: Integer; Iter:Integer=0);
var
  i:Int32;
  Matrix:T2DIntArray;
begin
  if not(Self.IsLoaded('TRafBitmap.Blur()')) then Exit;
  if (BlurSize < 3) or (BlurSize mod 2 = 0) then
  begin
    {$IFDEF ERR_REAL_EXCEPTION}
      RaiseException('TRafBitmap.Blur() expects an odd number greater then one.');
    {$ELSE}
      RaiseWarning('TRafBitmap.Blur() expects an odd number greater then one.', ERR_WARNING);
    {$ENDIF}
    Exit;
  end;

  Matrix := Self.ToMatrix();
  for i:=0 to Iter do
    Matrix := se.ImBlurFilter(Matrix, BlurSize);
  Self.FromMatrix(Matrix); 
end;


procedure TRafBitmap.Median(MedianSize: Integer);
var
  Matrix:T2DIntArray;
begin
  if not(Self.IsLoaded('TRafBitmap.Median()')) then Exit;
  if (Mediansize < 3) or (Mediansize mod 2 = 0) then
  begin
    {$IFDEF ERR_REAL_EXCEPTION}
      RaiseException('TRafBitmap.Median() expects an odd number greater then one.');
    {$ELSE}
      RaiseWarning('TRafBitmap.Median() expects an odd number greater then one.', ERR_WARNING);
    {$ENDIF}
    Exit;
  end;

  Matrix := Self.ToMatrix();
  Matrix := se.ImMedianFilter(Matrix, MedianSize);
  Self.FromMatrix(Matrix); 
end;


procedure TRafBitmap.Brightness(Amount:Extended; Legacy:Boolean);
var
  Matrix:T2DIntegerArray;
begin
  Matrix := Self.ToMatrix();
  Matrix := se.ImBrighten(Matrix, Amount, Legacy);
  Self.FromMatrix(Matrix);
end;


(*=============================================================================|
 Other functinality
|=============================================================================*)
procedure TRafBitmap.Debug();
begin
  if Self.Loaded then
  begin
    DisplayDebugImgWindow(Self.Width,Self.Height);
    DrawBitmapDebugImg(Self.Bitmap);
  end else
    RaiseWarning('TRafBitmap.Debug(), bitmap is not initalized.', ERR_NOTICE);
end;


function TRafBitmap.ToString(): String;
begin
  if Self.Loaded then
  begin
    Result := CreateBitmapString(Self.Bitmap);
  end else
    RaiseWarning('TRafBitmap.ToString(), bitmap is not initalized.', ERR_NOTICE);
end;


procedure TRafBitmap.Free();
begin
  if Self.Loaded then
  begin
    FreeBitmap(Self.Bitmap);
    Self.Width  := 0;
    Self.Height := 0;
    Self.Loaded := False;
  end else
    RaiseWarning('TRafBitmap.Free(), bitmap is not initalized.', ERR_NOTICE);
end;  


function TRafBitmap.IsLoaded(CallFrom:String; RaiseErr:Boolean=True): Boolean;
begin
  Result := Self.Loaded;
  if not(Result) then
  begin
    RaiseWarning(CallFrom+', bitmap is not initalized.', ERR_NOTICE);
  end;
end;

{$ENDIF}