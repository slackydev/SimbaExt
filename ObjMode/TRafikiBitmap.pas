{$IFDEF LAPE}
(*=============================================================================|
 Object oriented wrapper for Simba Bitmaps
|=============================================================================*)
{!DOCTOPIC}{ 
  TRafBitmap module
}

{!DOCREF} {
  @method: type TRafBitmap = record ... end;
  @desc: 
    Object oriented wrapper for Simba Bitmaps, with some extra stuff..
    Definition:
    [code=pascal]
    TRafBitmap = record
      Bitmap: Integer;
      Width, Height: Integer;
      Loaded:Boolean;
    end;
    [/code]
  
}
type
  TRafBitmap = record
    Bitmap: Integer;
    Width, Height: Integer;
    Loaded:Boolean;
  end;


(*=============================================================================|
 Regular intilaization
|=============================================================================*)

{!DOCREF} {
  @method: procedure TRafBitmap.Create(W, H: Integer; Str:String);
  @desc: ...
}
procedure TRafBitmap.Create(W, H: Integer; Str:String);
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := BitmapFromString(W,H,Str);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Create(W,H: Integer); overload;
  @desc: ...
}
procedure TRafBitmap.Create(W,H: Integer); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := CreateBitmap(W,H);
  Self.Loaded := True;
  Self.Width  := W;
  Self.Height := H;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Create(SimbaBitmap:Integer); overload;
  @desc: Allows you to create a TRafBitmap-instance from a Simba-bitmap
}
procedure TRafBitmap.Create(SimbaBitmap:Integer); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := SimbaBitmap;
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Open(ImgPath:String);
  @desc: Open a image file from anywhere on your computer
}
procedure TRafBitmap.Open(ImgPath:String);
begin
  if Self.Loaded then Self.Free();
  try
    Self.Bitmap := LoadBitmap(ImgPath);
    GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
    Self.Loaded := True;
  except
    if not(FileExists(ImgPath)) then
      RaiseWarning('File "'+ImgPath+'" does not exist.', ERR_WARNING)
    else
      RaiseWarning('Unexpected error in "TRafBitmap.Open()".', ERR_WARNING);
  end;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.FromClient();
  @desc: Loads the whole client bitmap in to this image. If this image is already in use it will be freed first.
}
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


{!DOCREF} {
  @method: procedure TRafBitmap.FromClient(X1,Y1,X2,Y2:Int32); overload;
  @desc: 
    Loads the client bitmap in to this image. If this image is already in use it will be freed first.
    Allows you to target a box of the client.
}
procedure TRafBitmap.FromClient(X1,Y1,X2,Y2:Int32); overload;
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
 Saving bitmap
|=============================================================================*)

{!DOCREF} {
  @method: function TRafBitmap.Save(ImgPath:String): TRafBitmap;
  @desc: ...
}
function TRafBitmap.Save(ImgPath:String): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.Save()')) then Exit;
  SaveBitmap(Self.Bitmap, ImgPath);
end;



(*=============================================================================|
 General functinality
|=============================================================================*)

{!DOCREF} {
  @method: procedure TRafBitmap.SetSize(NewWidth,NewHeight:Integer);
  @desc: Will increse, or decrease the size of the image, it the size is increased, the image will be extended with a black background.
}
procedure TRafBitmap.SetSize(NewWidth,NewHeight:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.SetSize()')) then Exit;
  SetBitmapSize(Self.Bitmap,NewWidth,NewHeight);
  Self.Width  := NewWidth;
  Self.Height := NewHeight;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Clear(Color:Integer);
  @desc: Clears the image and paints over with the given color.
}
procedure TRafBitmap.Clear(Color:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Clear()')) then Exit;
  FastDrawClear(Self.Bitmap, Color);
end;


{!DOCREF} {
  @method: function TRafBitmap.Clone(): TRafBitmap;
  @desc: Returns a copy of the image
}
function TRafBitmap.Clone(): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.Clone()')) then Exit;
  Result.Bitmap := CopyBitmap(Self.Bitmap);
  Result.Width  := Self.Width;
  Result.Height := Self.Height;
  Result.Loaded := True;
end;


{!DOCREF} {
  @method: function TRafBitmap.Crop(X1,Y1,X2,Y2:Integer): TRafBitmap;
  @desc:   Crops the image down to the given bounds. 
}
function TRafBitmap.Crop(X1,Y1,X2,Y2:Integer): TRafBitmap;
var m:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Crop()')) then Exit;
  M := Self.ToMatrix().GetArea(x1,y1,x2,y2);
  Result.FromMatrix(M);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.LazyCrop(X1,Y1,X2,Y2:Integer);
  @desc: 
    Crops the image down to the given bounds. 
    [note]Modifies the image, does not make a copy[/note]
}
procedure TRafBitmap.LazyCrop(X1,Y1,X2,Y2:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Crop()')) then Exit;
  CropBitmap(Self.Bitmap, X1,Y1,X2,Y2);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
end;


{!DOCREF} {
  @method: function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
  @desc: Returns all the pixels in the 'TPA' from the image
}
function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.GetPixels()')) then Exit;
  Result := FastGetPixels(Self.Bitmap, TPA);
end;


{!DOCREF} {
  @method: function TRafBitmap.Pixel(x,y:Integer): Integer;
  @desc: Gets the value at the given pixel-coordinate
}
function TRafBitmap.Pixel(x,y:Integer): Integer;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  Result := FastGetPixel(Self.Bitmap, x,y);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Pixel(x,y, color:Integer); overload;
  @desc: Sets the given pixel-coordinate to the value 'color'
}
procedure TRafBitmap.Pixel(x,y, color:Integer); overload;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  FastSetPixel(Self.Bitmap, x,y, color);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Int32);
  @desc: Sets all the given pixels to value 'color'
}
procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Int32);
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
  //DrawTPABitmap(Self.Bitmap, TPA, color);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.SetPixelsEx(TPA:TPointArray; Color:Integer);
  @desc: Sets all the given pixels to values in 'colors'
}
procedure TRafBitmap.SetPixelsEx(TPA:TPointArray; Colors:TIntArray);
begin
  if not(Self.IsLoaded('TRafBitmap.SetPixelsEx()')) then Exit;
  FastSetPixels(Self.Bitmap, TPA, Colors);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.ReplaceColor(OldColor, NewColor: Int32);
  @desc: Replaces all the occurances of OldColor with NewColor
}
procedure TRafBitmap.ReplaceColor(OldColor, NewColor: Int32);
begin
  if not(Self.IsLoaded('TRafBitmap.ReplaceColor()')) then Exit;
  FastReplaceColor(Self.Bitmap, OldColor, NewColor);
end;


{!DOCREF} {
  @method: function TRafBitmap.ToMatrix(): TIntMatrix;
  @desc: Returns a 2D matrix-representation of the image
}
function TRafBitmap.ToMatrix(): TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.ToMatrix()')) then Exit;
  Result := BitmapToMatrix(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.FromMatrix(Matrix: TIntMatrix);
  @desc: Copys the data from the 2D matrix and writes it to the image
}
procedure TRafBitmap.FromMatrix(Matrix: TIntMatrix);
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


{!DOCREF} {
  @method: function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Tolerance:Integer): Boolean;
  @desc: 
    Searches for the given 'color' in the bitmap.
    [note]@method is using a deprecated function. Disable warnings might be preferable '$DEFINE ERR_HIDE_ALL'[/note]  
  
}
function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Tolerance:Integer): Boolean;
var 
  Matrix: TIntMatrix;
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
  Result := exp_ImFindColorTolEx(Matrix, TPA, Color, Tolerance);
  SetLength(Matrix, 0);
  if not(Result) then Exit;
  if (Area.X1=0) and (Area.Y1 = 0) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;


{!DOCREF} {
  @method: function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
  @desc: 
    Searches for the given 'color' in the bitmap.
    [note]@method is using a deprecated function. Disable warnings might be preferable '$DEFINE ERR_HIDE_ALL'[/note]  
}
function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
begin
  Result := Self.FindColorTol(TPA, Color, Area, 0);
end;


{!DOCREF} {
  @method: function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
  @desc: 
    Searches for the given 'color' in the bitmap.
    [note]@method is using a deprecated function. Disable warnings might be preferable '$DEFINE ERR_HIDE_ALL'[/note]  
  
}
function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
begin
  Result := Self.FindColorTol(TPA, Color, IntToBox(0,0,self.width,self.height), 0);
end;



(*=============================================================================|
 Transformations
|=============================================================================*)
{!DOCREF} {
  @method: procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
  @desc: Simple and quick bitmap resizing using NN.
}
procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Resize()')) then Exit;
  ResizeBitmapEx(Self.Bitmap, RM_Nearest, NewWidth, NewHeight);
  Self.Width := NewWidth;
  Self.Height := NewHeight;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:TResizeAlgo);
  @desc: Allows you to resize the bitmap by not just using nearest neighbor, but also BICUBIC, and BILINEAR interpolation
}
procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:TResizeAlgo);
var
  Matrix:T2DIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.ResizeEx()')) then Exit;
  Matrix := Self.ToMatrix();
  exp_ImResize(Matrix, NewWidth, NewHeight, Resampler);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Rotate(const Degrees:Extended);
  @desc: Rotates the bitmap by the given angle using nearest neighbor interpolation
}
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


{!DOCREF} {
  @method: function TRafBitmap.RotateCopy(Degrees:Extended): TRafBitmap;
  @desc: Rotates a copy of the bitmap by the given angle using nearest neighbor interpolation
}
function TRafBitmap.RotateCopy(Degrees:Extended): TRafBitmap;
begin
  if not(Self.IsLoaded('TRafBitmap.RotateCopy()')) then Exit;
  Result.Bitmap := RotateBitmap(Self.Bitmap, Radians(Degrees));
  GetBitmapSize(Result.Bitmap, Result.Width, Result.Height);
  Result.Loaded := True;
end;


{!DOCREF} {
  @method: function TRafBitmap.Flip(Horizontal:Boolean): TRafBitmap;
  @desc: Flips the bitmap Left->Right or Top->Down.
}
function TRafBitmap.Flip(Horizontal:Boolean): TRafBitmap;
var 
  method: TBmpMirrorStyle;
begin
  if not(Self.IsLoaded('TRafBitmap.Flip()')) then Exit;
  case Horizontal of
    True: method := MirrorWidth;
    False:method := MirrorHeight;
  end;
  Result.Bitmap := CreateMirroredBitmapEx(Self.Bitmap, method);
  GetBitmapSize(Result.Bitmap, Result.Width, Result.Height);
  Result.Loaded := True;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Invert();
  @desc: Inverts the bitmap
}
procedure TRafBitmap.Invert();
begin
  if not(Self.IsLoaded('TRafBitmap.Invert()')) then Exit;
  InvertBitmap(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Blur(BlurSize: Integer; Iter:Integer=0);
  @desc: Allows you to blur the bitmap 'iter' times, using a block of the size 'blursize'
}
procedure TRafBitmap.Blur(BlurSize: Integer; Iter:Integer=0);
var
  i:Int32;
  Matrix:TIntMatrix;
  function BlurMore(Mat:TIntMatrix; Box:Int32):TIntMatrix;
  begin
    Result := exp_ImBlurFilter(Mat, Box);
  end;
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
  WriteLn(Length(Matrix));
  for i:=0 to Iter do
    Matrix := BlurMore(Matrix, BlurSize);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Median(MedianSize: Integer);
  @desc: ...
}
procedure TRafBitmap.Median(MedianSize: Integer);
var
  Matrix:TIntMatrix;
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

  Matrix := exp_ImMedianFilter(Self.ToMatrix(), MedianSize);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Brightness(Amount:Extended; Legacy:Boolean);
  @desc: Allows you to modify the brightness of the bitmap
}
procedure TRafBitmap.Brightness(Amount:Extended; Legacy:Boolean=True);
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Brightness()')) then Exit;
  Matrix := Self.ToMatrix();
  
  Matrix := exp_ImBrighten(Matrix, Amount, Legacy);
  Self.FromMatrix(Matrix);
end;


{!DOCREF} {
  @method: function TRafBitmap.Blend(Other:TRafBitmap; Alpha:Single): TRafBitmap;
  @desc: Belnds the two images in to one. Alpha must be in range of 0-1.
}
function TRafBitmap.Blend(Other:TRafBitmap; Alpha:Single): TRafBitmap;
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Blend()')) then 
    Exit;
  
  if not(Other.Loaded) then 
    RaiseException(erException, '''Other'' bitmap is not loaded.');
    
  if not(Other.Width=Self.Width) or not(Other.Height=Self.Height) then 
    RaiseException(erException, 'Bitmaps must have the same size'); 

  Matrix := exp_ImBlend(Self.ToMatrix(), Other.ToMatrix(), Alpha);
  Result.FromMatrix(Matrix);
end;


(*=============================================================================|
 Other functinality
|=============================================================================*)
{!DOCREF} {
  @method: procedure TRafBitmap.Debug();
  @desc: Debugs the bitmap
}
procedure TRafBitmap.Debug();
begin
  if not(Self.IsLoaded('TRafBitmap.Debug()')) then Exit;
  DisplayDebugImgWindow(Self.Width,Self.Height);
  DrawBitmapDebugImg(Self.Bitmap);
end;


{!DOCREF} {
  @method: function TRafBitmap.ToString(): String;
  @desc: Converts the bitmap in to an encoded string format. This format can then be used with 'TRafBitmap.Create(W,H:Int32; Str:String);' to recreate the bitmap
}
function TRafBitmap.ToString(): String;
begin
  if not(Self.IsLoaded('TRafBitmap.ToString()')) then Exit;
  Result := CreateBitmapString(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Free();
  @desc: Releases the bitmap
}
procedure TRafBitmap.Free();
begin
  if not(Self.IsLoaded('TRafBitmap.Free()')) then Exit;
  FreeBitmap(Self.Bitmap);
  Self.Width  := 0;
  Self.Height := 0;
  Self.Loaded := False;
end;  


function TRafBitmap.IsLoaded(CallFrom:String; RaiseErr:Boolean=True): Boolean;
begin
  Result := Self.Loaded;
  if not(Result) then
  begin
    RaiseWarning('Bitmap is not initalized in "'+CallFrom+'"', ERR_NOTICE);
  end;
end;

{$ENDIF}
