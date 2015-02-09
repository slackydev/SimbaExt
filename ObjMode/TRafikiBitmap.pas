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

var
  //Search paths
  SE_BITMAP_PATHS = ['Tests/', 'Scripts/', 'Includes/'];

  //Log every created bitmap, free if not freed once script ends.
  SE_BITMAP_LOG: TIntArray;

{$IFNDEF CodeInsight}
procedure __LOG_BITMAP(B:Int32);
begin
  SE_BITMAP_LOG.Append(B);
end;

function __FIND_BITMAP_PATH(var F:String): Boolean;
var i:Int32;
begin
  Result := FileExists(F);
  if Result then Exit();
  if Pos(':',F) <> 0 then Exit();

  for i:=0 to High(SE_BITMAP_PATHS) do
  begin
    Result := FileExists(SE_BITMAP_PATHS[i]+F);
    if Result then
    begin
      F := SE_BITMAP_PATHS[i]+F;
      Exit();
    end;
  end;
end;
{$ENDIF}

type
  PRafBitmap = ^TRafBitmap;
  TRafBitmap = record
    Bitmap: Integer;
    Width, Height: Integer;
    Loaded:Boolean;
  end;

  TRafDraw = record
    BMP:PRafBitmap;
    Loaded:Boolean;
  end;


(*=============================================================================|
 Initilaization
|=============================================================================*)

{!DOCREF} {
  @method: procedure TRafBitmap.Create(W,H: Integer);
  @desc: Creates a bitmap of the given size.
}
procedure TRafBitmap.Create(W,H: Integer);
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := CreateBitmap(W,H);
  Self.Loaded := True;
  Self.Width  := W;
  Self.Height := H;
  __LOG_BITMAP(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Create(W,H: Integer; Str:String); overload;
  @desc: Creates a bitmap from a string.
}
procedure TRafBitmap.Create(W, H: Integer; Str:String); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := BitmapFromString(W,H,Str);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
  __LOG_BITMAP(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Create(W,H: Integer; Str:String); overload;
  @desc: Creates a bitmap from a string.
}
procedure TRafBitmap.Create(W, H: Integer; Color:Int32); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := CreateBitmap(W,H);
  Self.Loaded := True;
  Self.Width  := W;
  Self.Height := H;
  Self.Clear(Color);
  __LOG_BITMAP(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Create(SimbaBitmap:Integer); overload;
  @desc: Create a TRafBitmap-instance from a Simba-bitmap.
}
procedure TRafBitmap.Create(SimbaBitmap:Integer); overload;
begin
  if Self.Loaded then Self.Free();
  Self.Bitmap := SimbaBitmap;
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  Self.Loaded := True;
  __LOG_BITMAP(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Open(ImgPath:String);
  @desc: Open a image file from anywhere on your computer
}
procedure TRafBitmap.Open(ImgPath:String);
begin
  if not(__FIND_BITMAP_PATH(ImgPath)) then
  begin
    RaiseWarning('TRafBitmap.Open(): File "'+ImgPath+'" does not exist.', ERR_WARNING);
    Exit();
  end
  else if Self.Loaded then Self.Free();
  try
    Self.Bitmap := LoadBitmap(ImgPath);
    GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
    Self.Loaded := True;
    __LOG_BITMAP(Self.Bitmap);
  except
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
  __LOG_BITMAP(Self.Bitmap);
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
  __LOG_BITMAP(Self.Bitmap);
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
  @method: procedure TRafBitmap.SetSize(NewWidth,NewHeight:Int32);
  @desc: Will increse, or decrease the size of the image, it the size is increased, the image will be extended with a black background.
}
procedure TRafBitmap.SetSize(NewWidth,NewHeight:Int32);
begin
  if not(Self.IsLoaded('TRafBitmap.SetSize()')) then Exit;
  SetBitmapSize(Self.Bitmap, NewWidth, NewHeight);
  Self.Width  := NewWidth;
  Self.Height := NewHeight;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Clear(Color:Int32);
  @desc: Clears the image and paints over with the given color.
}
procedure TRafBitmap.Clear(Color:Int32);
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
  @method: function TRafBitmap.Crop(X1,Y1,X2,Y2:Int32): TRafBitmap;
  @desc:   Crops the image down to the given bounds. 
}
function TRafBitmap.Crop(X1,Y1,X2,Y2:Int32): TRafBitmap;
var
  m:TIntMatrix;
  _x1,_y1,_x2,_y2: Int32;
begin
  if not(Self.IsLoaded('TRafBitmap.Crop()')) then Exit;
  _x2 := Min(x2,Self.Width-1);
  _y2 := Min(y2,Self.height-1);
  _x1 := Min(Max(x1,0), Self.Width-1);
  _y1 := Min(Max(y1,0), Self.height-1);

  M := Self.ToMatrix().Area(_x1,_y1,_x2,_y2);
  Result.FromMatrix(M);

  if (x2 <> _x2) or (y2 <> _y2) or (x1 <> _x1) or (y1 <> _y1) then
  begin
    RaiseWarning(
        Format(
            'TRafBitmap.Crop(): Bounds (%d,%d,%d,%d) exceeds bitmap-size (%d,%d)',
            [x1,y1,x2,y2, self.width, self.height]
        ), ERR_WARNING);
    Result.SetSize(x2-x1+1, y2-y1+1);
  end;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.LazyCrop(X1,Y1,X2,Y2:Int32);
  @desc: 
    Crops the image down to the given bounds. 
    [note]Modifies the image, does not make a copy[/note]
}
procedure TRafBitmap.LazyCrop(X1,Y1,X2,Y2:Int32);
begin
  if not(Self.IsLoaded('TRafBitmap.LazyCrop()')) then Exit;
  CropBitmap(Self.Bitmap, X1,Y1,X2,Y2);
  GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
end;


{!DOCREF} {
  @method: function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
  @desc: Returns all the pixels in the `TPA` from the image
}
function TRafBitmap.GetPixels(TPA:TPointArray): TIntegerArray;
begin
  if not(Self.IsLoaded('TRafBitmap.GetPixels()')) then Exit;
  Result := FastGetPixels(Self.Bitmap, TPA);
end;


{!DOCREF} {
  @method: function TRafBitmap.Pixel(x,y:Int32): Integer;
  @desc: Gets the color at the given pixel
}
function TRafBitmap.Pixel(x,y:Integer): Integer;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  Result := FastGetPixel(Self.Bitmap, x,y);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Pixel(x,y, color:Int32; Alpha:Single=1); overload;
  @desc: Sets the given pixels value to the value `color`
}
procedure TRafBitmap.Pixel(x,y, color:Int32); overload;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  FastSetPixel(Self.Bitmap, x,y, color);
end;

//....
procedure TRafBitmap.Pixel(x,y:Int32; Color:Int32; Alpha:Single); overload;
var
  bg,fg,c:TRGB32;
begin
  if not(Self.IsLoaded('TRafBitmap.Pixel()')) then Exit;
  fg := TRGB32(color);
  bg := TRGB32(FastGetPixel(Self.Bitmap,x,y));
  c.r := Round(alpha * fg.R + (1.0-alpha) * bg.R);
  c.g := Round(alpha * fg.G + (1.0-alpha) * bg.G);
  c.b := Round(alpha * fg.B + (1.0-alpha) * bg.B);
  FastSetPixel(Self.Bitmap,x,y,Int32(c));
end;  


{!DOCREF} {
  @method: procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Int32);
  @desc: Sets all the given pixels to value `color`
}
procedure TRafBitmap.SetPixels(TPA:TPointArray; Color:Int32; EIgnore:Boolean=True);
var
  i,x,y, Hi: Int32;
begin
  if not(Self.IsLoaded('TRafBitmap.SetPixels()')) then Exit;
  if EIgnore then
  begin
    Hi := High(TPA);
    if (Hi < -1) then Exit;
    for i := 0 to Hi do
    begin
      x := TPA[i].X;
      y := TPA[i].Y;
      if ((x >= 0) and (y >= 0) and
          (x < Self.Width) and (y < Self.Height)) then
        FastSetPixel(Self.Bitmap, x,y, color);
    end;
  end else
    DrawTPABitmap(Self.Bitmap, TPA, color);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.SetPixels(TPA:TPointArray; Colors:TIntArray); overload;
  @desc: Sets all the given pixels `TPA` to values in `colors`
}
procedure TRafBitmap.SetPixels(TPA:TPointArray; Colors:TIntArray); overload;
begin
  if not(Self.IsLoaded('TRafBitmap.SetPixels()')) then Exit;
  FastSetPixels(Self.Bitmap, TPA, Colors);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.ReplaceColor(OldColor, NewColor: Int32);
  @desc: Replaces all the occurances of `OldColor` with `NewColor`
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
    try
      DrawMatrixBitmap(Self.Bitmap, Matrix);
      GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
    except
      RaiseWarning('TRafBitmap.FromMatrix: Matrix is not initalized', ERR_WARNING);
    end;
  end else
  begin
    Self.Create(1,1);
    DrawMatrixBitmap(Self.Bitmap, Matrix);
    GetBitmapSize(Self.Bitmap, Self.Width, Self.Height);
  end;
end;


{!DOCREF} {
  @method: function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Similarity:Single): Boolean;
  @desc: 
    Searches for the given `color` in the bitmap.
    `Similarity` ranges from 0.0 to 1.0
}
function TRafBitmap.FindColorTol(var TPA:TPointArray; Color:Integer; Area:TBox; Similarity:Single): Boolean;
var 
  Matrix: TIntMatrix;
  Corr: TFloatMatrix;
  OldFunc:TColorDistFunc;
  OldSpace: Int32;
begin
  Result := False;
  if not(Self.IsLoaded('TRafBitmap.FindColorTol()')) then Exit;
  
  if (Area.X2 >= Self.Width) then Area.X2 := Self.Width - 1
  else if (Area.X2 <= -1) then Area.X2 := Self.Width - Area.x2;
  
  if (Area.Y2 >= Self.Height) then Area.Y2 := Self.Height - 1
  else if (Area.Y2 <= -1) then Area.Y2 := Self.Height - Area.y2;
  
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;
  
  Matrix := Self.ToMatrix().Area(Area.x1,Area.y1,Area.x2,Area.y2);

  //Finder.GetCompareInfo(@OldFunc,OldSpace);
  //Finder.SetComparePreset(ECD_RGB_NORMED);
  Corr := Finder.MatchColor(Matrix, Color);
  //Finder.SetCompareInfo(@OldFunc,OldSpace);

  TPA := Corr.Indices(Similarity, CMP_GTE);
  Result := Length(TPA) < 0;

  if not(Result) or ((Area.X1=0) and (Area.Y1 = 0)) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;


{!DOCREF} {
  @method: function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
  @desc: 
    Searches for the given 'color' in the bitmap.
    [note]method is using a deprecated function. That means params might change in the future[/note]  
}
function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer; Area:TBox): Boolean;
begin
  Result := Self.FindColorTol(TPA, Color, Area, 0.99);
end;


{!DOCREF} {
  @method: function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
  @desc: 
    Searches for the given 'color' in the bitmap.
    [note]method is using a deprecated function. That means params might change in the future[/note]  
  
}
function TRafBitmap.FindColor(var TPA:TPointArray; Color:Integer): Boolean; overload;
begin
  Result := Self.FindColorTol(TPA, Color, IntToBox(0,0,self.width,self.height), 0.99);
end;



(*=============================================================================|
 Transformations
|=============================================================================*)
{!DOCREF} {
  @method: procedure TRafBitmap.Grayscale();
  @desc: Converts the image to grayscale.
}
procedure TRafBitmap.Grayscale();
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Grayscale()')) then Exit;
  Matrix := Self.ToMatrix();
  se.ImGrayscale(Matrix, Matrix);
  Self.FromMatrix(Matrix);
end;


{!DOCREF} {
  @method: function TRafBitmap.Sample(Scale:Int32): TRafBitmap;
  @desc: Simple and quick bitmap downsampling.
}
function TRafBitmap.Sample(Scale:Int32): TRafBitmap;
var Mat:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Sample()')) then Exit;
  Mat := se.ImSample(Self.ToMatrix(), Scale);
  Result.FromMatrix(Mat);
end;


{!DOCREF} {
  @method: function TRafBitmap.LazySample(Scale:Int32);
  @desc: Simple and quick bitmap downsampling.
}
procedure TRafBitmap.LazySample(Scale:Int32);
var Mat:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Sample()')) then Exit;
  Mat := se.ImSample(Self.ToMatrix(), Scale);
  Self.FromMatrix(Mat);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
  @desc: Simple and quick bitmap resizing using bilinear filter.
}
procedure TRafBitmap.Resize(NewWidth, NewHeight:Integer);
begin
  if not(Self.IsLoaded('TRafBitmap.Resize()')) then Exit;
  ResizeBitmapEx(Self.Bitmap, RM_Bilinear, NewWidth, NewHeight);
  Self.Width := NewWidth;
  Self.Height := NewHeight;
end;


{!DOCREF} {
  @method: procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:EResizeAlgo=ERA_BILINEAR);
  @desc: Allows you to resize the bitmap by not just using nearest neighbor, but also BICUBIC, and BILINEAR interpolation
}
procedure TRafBitmap.ResizeEx(NewWidth, NewHeight:Integer; Resampler:EResizeAlgo=ERA_BILINEAR);
var Mat:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.ResizeEx()')) then Exit;
  Mat := Self.ToMatrix();
  se.ImResize(Mat, NewWidth, NewHeight, Resampler);
  Self.FromMatrix(Mat); 
end;


{!DOCREF} {
  @method: function TRafBitmap.Rotate(Angle:Extended; Expand:Boolean; Smooth:Boolean=True): TRafBitmap;
  @desc: 
    Rotates a copy of the bitmap by the given angle. 
    [params]
    [b]Algle:[/b] The amount to rotate the bitmap, defined in radians
    [b]Expand:[/b] `True` means that the result is expanded to fit the rotated image. `False` keeps original size.
    [b]Smooth:[/b] `True` will use bilinear interpolation, while `False` will use nearest neighbor.
    [/params]
}
function TRafBitmap.Rotate(Angle:Extended; Expand:Boolean; Smooth:Boolean=True): TRafBitmap;
var Mat:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.RotateCopy()')) then Exit;
  Mat := se.ImRotate(Self.ToMatrix(), Angle, Expand, Smooth);
  Result.FromMatrix(Mat);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.LazyRotate(Angle:Extended; Expand:Boolean; Smooth:Boolean=True);
  @desc: 
    Rotates a copy of the bitmap by the given angle. 
    [params]
    [b]Algle:[/b] The amount to rotate the bitmap, defined in radians
    [b]Expand:[/b] `True` means that the result is expanded to fit the rotated image. `False` keeps original size.
    [b]Smooth:[/b] `True` will use bilinear interpolation, while `False` will use nearest neighbor.
    [/params]
}
procedure TRafBitmap.LazyRotate(Angle:Extended; Expand:Boolean; Smooth:Boolean=True);
var Mat:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.RotateCopy()')) then Exit;
  Mat := se.ImRotate(Self.ToMatrix(), Angle, Expand, Smooth);
  Self.FromMatrix(Mat);
end;


{!DOCREF} {
  @method: function TRafBitmap.Flip(Horizontal:Boolean): TRafBitmap;
  @desc: Flips the bitmap Left->Right `Horizontal=True` or Top->Down `Horizontal=False`
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
  @desc: Inverts the colors in the bitmap
}
procedure TRafBitmap.Invert();
begin
  if not(Self.IsLoaded('TRafBitmap.Invert()')) then Exit;
  InvertBitmap(Self.Bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Blur(Radius: Integer; Iter:Integer=1);
  @desc: Allows you to blur the bitmap `Iter` times, with a radius of the size `Radius`
}
procedure TRafBitmap.Blur(Radius: Integer; Iter:Integer=1);
var
  i:Int32;
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Blur()')) then Exit;

  Matrix := Self.ToMatrix();
  for i:=1 to Iter do
    se.ImBlur(Matrix, Matrix, Radius);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.MedianBlur(Radius: Integer);
  @desc: Computes the median of the neighborhood of radius `Radius` for each pixel in the image and sets the result pixel to that value. 
}
procedure TRafBitmap.Median(Radius: Integer);
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Median()')) then Exit;

  Matrix := se.ImMedianBlur(Self.ToMatrix(), Radius);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.GaussianBlur(Radius: Integer; Sigma:Single=1.5);
  @desc: Appends a gaussion blur to the bitmap, with a radius of the size `Radius`. Sigma is a modifier, higher sigma = less focus on center.
}
procedure TRafBitmap.GaussianBlur(Radius: Integer; Sigma:Single=1.5);
var i:Int32;
    Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.GaussianBlur()')) then Exit();
  SetLength(Matrix, Self.Height,Self.Width);
  se.ImGaussBlur(Self.ToMatrix(), Matrix, Radius, sigma);
  Self.FromMatrix(Matrix); 
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Brightness(Amount:Extended);
  @desc: 
    Allows you to modify the brightness of the bitmap.
}
procedure TRafBitmap.Brightness(Amount:Extended);
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Brightness()')) then Exit;
  Matrix := Self.ToMatrix();
  
  Matrix := se.ImBrighten(Matrix, Amount);
  Self.FromMatrix(Matrix);
end;


{!DOCREF} {
  @method: function TRafBitmap.Blend(Other:TRafBitmap; Alpha:Single): TRafBitmap;
  @desc: 
    Belnds the two images in to one. Alpha must be in range of `0.0-1.0`.
    Both images must also be the exact same size.
}
function TRafBitmap.Blend(Other:TRafBitmap; Alpha:Single): TRafBitmap;
var
  Matrix:TIntMatrix;
begin
  if not(Self.IsLoaded('TRafBitmap.Blend()')) then 
    Exit;
  
  if not(Other.Loaded) then 
    RaiseException(erException, '"Other" bitmap is not loaded.');
    
  if not(Other.Width=Self.Width) or not(Other.Height=Self.Height) then 
    RaiseException(erException, 'Bitmaps must have the same size'); 

  Matrix := se.ImBlend(Self.ToMatrix(), Other.ToMatrix(), Alpha);
  Result.FromMatrix(Matrix);
end;


{!DOCREF} {
  @method: function TRafBitmap.Draw(Other:TRafBitmap; Pos:TPoint; AutoResize:Boolean=False): TRafBitmap;
  @desc: Draws the `other` image to the current image-instance at the given position `Pos`. If `AutoResize` is `True`, it will resize the current image-instance automaticly if needed.
}
function TRafBitmap.Draw(Other:TRafBitmap; Pos:TPoint; AutoResize:Boolean=False): TRafBitmap;
var nw,nh:Int32;
begin
  if not(Self.IsLoaded('TRafBitmap.Blend()')) then 
    Exit;
  
  if AutoResize then
  begin
    NW := Self.Width;
    NH := Self.Height;
    if Pos.x+Other.Width >= Self.Width then   NW := Pos.x+Other.Width;
    if Pos.y+Other.Height >= Self.Height then NH := Pos.y+Other.Height;
    if (NH > Self.Height) or (NW > Self.Width) then
      Self.SetSize(NW, NH);
  end;
  FastDrawTransparent(Pos.x, Pos.y, other.bitmap, self.bitmap);
end;


{!DOCREF} {
  @method: procedure TRafBitmap.Pad(Pixels:Int32);
  @desc:
    Pads the contents of the image `Pixels` from the center.
}
procedure TRafBitmap.Pad(Pixels:Int32);
var
  TMP:TRafBitmap;
begin
  TMP := Self.Clone();
  Self.SetSize(Self.Width+(2*Pixels), Self.Height+(2*Pixels));
  Self.Clear(0);
  Self.Draw(TMP,[Pixels,Pixels],True);
  TMP.Free();
end;


{!DOCREF} {
  @method: procedure TRafBitmap.ThresholdAdaptive(Alpha,Beta:Int32; Method:EThreshAlgo; c:Int32);
  @desc:
    Threshold the image
}
procedure TRafBitmap.ThresholdAdaptive(Alpha,Beta:Int32; Method:EThreshAlgo; c:Int32);
var
  Matrix:TIntMatrix;
begin
  Matrix := Self.ToMatrix();
  se.ImThresholdAdaptive(Matrix,Alpha,Beta,Method,c);
  Self.FromMatrix(Matrix);
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
  @desc: Converts the bitmap in to an encoded string format. This format can then be used with `TRafBitmap.Create(W,H:Int32; Str:String);` to recreate the bitmap.
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
  try
    if not(Self.IsLoaded('TRafBitmap.Free()')) then Exit;
    SE_BITMAP_LOG.Remove(Self.Bitmap);
    FreeBitmap(Self.Bitmap);
    Self.Width  := 0;
    Self.Height := 0;
    Self.Loaded := False;
  except
    RaiseWarning('TRafBitmap: Does not own the bitmap', ERR_WARNING);
  end;
end;  


function TRafBitmap.IsLoaded(CallFrom:String; RaiseErr:Boolean=True): Boolean;
begin
  Result := Self.Loaded;
  if not(Result) then
    RaiseWarning(CallFrom+': Bitmap is not initalized', ERR_WARNING);

end;

{$IFNDEF CodeInsight}
procedure TRafBitmap_OnTerminate();
var i:Int32;
begin
  for i:=0 to High(SE_BITMAP_LOG) do
    FreeBitmap(SE_BITMAP_LOG[i]);
end;
{$ENDIF}

begin
  AddOnTerminate('TRafBitmap_OnTerminate');
end;
