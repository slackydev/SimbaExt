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
  Self.Bitmap := BitmapFromString(W,H,Str);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.Create(W,H: Integer); overload;
begin
  Self.Bitmap := CreateBitmap(W,H);
  Self.Loaded := True;
  Self.Width  := W;
  Self.Height := H;
end;


procedure TRafBitmap.Open(ImgPath:String);
begin
  Self.Bitmap := LoadBitmap(ImgPath);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.FromBitmap(Bitmap:Integer);
begin
  Self.Bitmap := Bitmap;
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.FromClient();
var 
  W,H:Integer;
begin
  GetClientDimensions(W,H);
  Self.Bitmap := BitmapFromClient(0,0,W-1,H-1); 
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


procedure TRafBitmap.FromClient(X1,Y1,X2,Y2:Integer); overload;
var 
  W,H:Integer;
begin
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
  SaveBitmap(Self.Bitmap, ImgPath);
end;

// SCAR Divi dummy methods:
function TRafBitmap.SaveToJpeg(ImgPath:String): TRafBitmap;
begin
  SaveBitmap(Self.Bitmap, ImgPath);
end;

function TRafBitmap.SaveToBmp(ImgPath:String): TRafBitmap;
begin
  SaveBitmap(Self.Bitmap, ImgPath);
end;

function TRafBitmap.SaveToPng(ImgPath:String): TRafBitmap;
begin
  SaveBitmap(Self.Bitmap, ImgPath);
end;



(*=============================================================================|
 General functinality
|=============================================================================*)
procedure TRafBitmap.SetSize(NewWidth,NewHeight:Integer);
begin
  SetBitmapSize(Self.Bitmap,NewWidth,NewHeight);
  Self.Width  := NewWidth;
  Self.Height := NewHeight;
end;


procedure TRafBitmap.Clear(Color:Integer);
begin
  FastDrawClear(Self.Bitmap, Color);
end;


function TRafBitmap.Clone(): TRafBitmap;
begin
  Result.Bitmap := CopyBitmap(Self.Bitmap);
  Result.Width  := Self.Width;
  Result.Height := Self.Height;
  Result.Loaded := True;
end;


procedure TRafBitmap.Crop(X1,Y1,X2,Y2:Integer);
begin
  CropBitmap(Self.Bitmap, X1,Y1,X2,Y2);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
end;


procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
begin
  ResizeBitmapEx(Self.Bitmap, RM_Nearest, NewWidth, NewHeight);
  Self.Width := NewWidth;
  Self.Height := NewHeight;
end;


procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:TBmpResizeMethod);
begin
  ResizeBitmapEx(Self.Bitmap, Resampler, NewWidth, NewHeight);
  Self.Width := NewWidth;
  Self.Height := NewHeight;
end;


function TRafBitmap.Rotate(Degrees:Extended): TRafBitmap;
var 
  Angle:Extended;
begin
  Angle := Radians(Degrees);
  Result.Bitmap := RotateBitmap(Self.Bitmap, Angle);
  GetBitmapSize(Result.Bitmap, Result.Width, Result.Height);
  Result.Loaded := True;
end;


function TRafBitmap.Flip(Horizontal:Boolean): TRafBitmap;
var 
  Method: TBmpMirrorStyle;
begin
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
  InvertBitmap(Self.Bitmap);
end;


function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
begin
  Result := FastGetPixels(Self.Bitmap, TPA);
end;


function TRafBitmap.Pixel(x,y:Integer): Integer;
begin
  Result := FastGetPixel(Self.Bitmap, x,y);
end;


procedure TRafBitmap.Pixel(x,y, color:Integer); overload
begin
  FastSetPixel(Self.Bitmap, x,y, color);
end;


procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Integer);
var
  i,x,y, Hi: Integer;
begin
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
  FastSetPixels(Self.Bitmap, TPA, Colors);
end;


procedure TRafBitmap.ReplaceColor(OldColor, NewColor: Integer);
begin
  FastReplaceColor(Self.Bitmap, OldColor, NewColor);
end;


function TRafBitmap.ToMatrix(): T2DIntegerArray;
begin
  Result := BitmapToMatrix(Self.Bitmap);
end;


procedure TRafBitmap.FromMatrix(Matrix: T2DIntegerArray);
begin
  if Self.Loaded then
  begin
    DrawMatrixBitmap(Self.Bitmap, Matrix);
  end else
  begin
    Self.Create(1,1);
    DrawMatrixBitmap(Self.Bitmap, Matrix);
  end;
end;


function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Tolerance:Integer): Boolean;
var 
  Img:T2DIntegerArray;
  tmp:TRafBitmap;
begin
  Result := False;
  if not(Self.Loaded) then Exit;
  
  if (Area.X2 >= Self.Width) then Area.X2 := Self.Width - 1
  else if (Area.X2 <= -1) then Area.X2 := Self.Width-Area.x2;
  
  if (Area.Y2 >= Self.Height) then Area.Y2 := Self.Height - 1
  else if (Area.Y2 <= -1) then Area.Y2 := Self.Height - Area.y2;
  
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;
  
  Tmp := Self.Clone();
  Tmp.Crop(Area.x1,Area.y1,Area.x2,Area.y2);
  Img := Tmp.ToMatrix();
  Result := XT_ImFindColorTolEx(Img, TPA, Color, Tolerance);
  SetLength(Img, 0);
  if not(Result) then Exit;
  if (Area.X1=0) and (Area.Y1 = 0) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
  Tmp.Free;
end;


function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
begin
  Result := Self.FindColorTol(TPA, Color, Area, 0);
end;


function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
begin
  Result := Self.FindColorTol(TPA, Color, IntToBox(0,0,-1,-1), 0);
end;


procedure TRafBitmap.Debug();
begin
  if Self.Loaded then
  begin
    DisplayDebugImgWindow(Self.Width,Self.Height);
    DrawBitmapDebugImg(Self.Bitmap);
  end;
end;


function TRafBitmap.ToString(): String;
begin
  if Self.Loaded then
  begin
    Result := CreateBitmapString(Self.Bitmap);
  end;
end;


procedure TRafBitmap.Free();
begin
  if Self.Loaded then
  begin
    FreeBitmap(Self.Bitmap);
    Self.Width  := 0;
    Self.Height := 0;
    Self.Loaded := False;
  end;
end;  

{$ENDIF}