{!DOCTOPIC} {
  Finder functions 
}
{$F-}

const
  TM_SQDIFF        = 0;
  TM_SQDIFF_NORMED = 1;
  TM_CCORR         = 2;
  TM_CCORR_NORMED  = 3;
  TM_CCOEFF        = 4;
  TM_CCOEFF_NORMED = 5; 

{-------------------------------------------------------------------------------]
 Raw base for MatchTemplate
[-------------------------------------------------------------------------------}
{$IFNDEF CODEINSIGHT}
type CVMat = record Data:Pointer; cols,rows:Int32; end; 

function __cvLoadFromMatrix(var Mat:TIntMatrix): CVMat;
var
  w,h,y:Int32;
  data:TIntArray;
begin
  SetLength(Data, Mat.Width()*Mat.Height());
  W := Mat.Width();
  H := Mat.Height();
  for y:=0 to H-1 do
    MemMove(Mat[y][0], data[y*W], 4*W);

  Result.Data := nil;
  cv_MatFromData(PChar(data), W,H, Result.Data);
  Result.Cols := W;
  Result.Rows := H;
  SetLength(data, 0);
end;


procedure __cvFreeMatrix(var Matrix:CVMat);
begin
  cv_FreeImage(Matrix.data);
  Matrix.cols := 0;
  Matrix.rows := 0;
end;


function __MatchTemplate(var img, templ:CVMat; Algo: Int8;
                         Normed:Boolean=True): TFloatMatrix;
var
  res:Pointer;
  Ptr:PFloat32;
  i,j,W,H:Int32;
begin
  if (img.data = nil) or (templ.Data = nil) then begin
    RaiseWarning('One or both the images are empty!', ERR_WARNING);
    Exit();
  end;

  if (templ.rows > img.rows) or (templ.cols > templ.cols) then begin
    RaiseWarning('Sub cannot be larger then Image', ERR_WARNING);
    Exit();
  end;

  W := img.cols - templ.cols + 1;
  H := img.rows - templ.rows + 1;
  SetLength(Result, H,W);

  Ptr := PFloat32(cv_MatchTemplate(img.Data, templ.Data, algo, normed, res));
  if (Ptr = nil) then Exit();

  for i:=0 to H-1 do
    MemMove(Ptr[i*W]^, Result[i][0], 4*W);

  cv_FreeImage(res);
end;
{$ENDIF}
{-------------------------------------------------------------------------------]
[-------------------------------------------------------------------------------}




{!DOCREF} {
  @method: function se.MatchColor(Image:TRafBitmap; Color:Int32; MatchAlgo:EColorDistance): TFloatMatrix;
  @desc: 
    Correlates the color with the given image and stores the comparison results in the `Result`.
    [params]
     Image:      Image to search in
     Color:      Color to search for
     MatchAlgo:  Algorithm used to compute difference
    [/params]
}
function SimbaExt.MatchColor(Image:TRafBitmap; Color:Int32; MatchAlgo: EColorDistance): TFloatMatrix; overload;
var
  Mat:TIntMatrix;
  OldFunc:TColorDistFunc;
  OldSpace: Int32;
begin
  Mat := Image.ToMatrix();
  Finder.GetCompareInfo(OldFunc,OldSpace);
  Finder.SetComparePreset(MatchAlgo);
  Result := Finder.MatchColor(Mat, Color);
  Finder.SetCompareInfo(OldFunc,OldSpace);
end;



{!DOCREF} {
  @method: function se.FindColorEx(var TPA:TPointArray; Color:Integer; Area:TBox; Similarity:Single; MatchAlgo:EColorDistance): Boolean;
  @desc:
    Search for a spesific color on your screen with a tolerance.[br]
    
    [params]
     TPA:        The resulting points
     Color:      The color to search for
     Area:       A `TBox` of where to search.
     Similarity: Depends on `MatchAlgo`, but for any NORMED method: `0.0` to `1.0` where `1.0` should be exact match.
     MatchAlgo:  Algorithm used to compute difference
    [/params]
}
function SimbaExt.FindColorEx(var TPA:TPointArray; Color:Int32; Area:TBox; Similarity:Single; MatchAlgo:EColorDistance): Boolean;
var 
  W,H:Int32;
  Corr: TFloatMatrix;
  Mat:TIntMatrix;
  BMP:TRafBitmap;
  OldFunc:TColorDistFunc;
  OldSpace: Int32;
begin
  Result := False;
  GetClientDimensions(W,H);
  
  if (Area.X2 <= -1) then Area.X2 := W-Area.X2;
  if (Area.Y2 <= -1) then Area.Y2 := H-Area.Y2;
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;

  BMP.FromClient(Area.X1,Area.Y1,Area.X2,Area.Y2);
  Mat := BMP.ToMatrix();
  BMP.Free();

  Finder.GetCompareInfo(OldFunc,OldSpace);
  Finder.SetComparePreset(MatchAlgo);
  Corr := Finder.MatchColor(Mat, Color);
  Finder.SetCompareInfo(OldFunc,OldSpace);

  TPA := Corr.Indices(Similarity, CMP_GTE);
  Result := Length(TPA) < 0;

  if not(Result) or ((Area.X1=0) and (Area.Y1 = 0)) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;
  
  

{!DOCREF} {
  @method: function se.FindTemplate(out PT:TPoint; Template:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean;
  @desc:
    Search for a template image on your screen with a tolerance, returns the point where it best matched.
    
    [params]
     PT:         Resulting point
     Template:   Template image to search for
     Area:       A `TBox` of where to search.
     Similarity: `-1.0` to `1.0` where `+/-1.0` should be exact match.    
     MatchAlgo:  Comparison algorithm -> `TM_SQDIFF_NORMED = 1`, `TM_CCORR_NORMED = 3`, `TM_CCOEFF_NORMED = 5` 
    [/params]
}
function SimbaExt.FindTemplate(out PT:TPoint; Template:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean;
var
  W,H:Int32;
  Corr:TFloatMatrix;
  Screen:TRafBitmap;
  templ,img:CVMat; 
begin
  PT := [-1,-1];
  if (MatchAlgo = 0) or (MatchAlgo = 2) or (MatchAlgo = 4) then begin
    RaiseWarning('se.FindTemplate only supports NORMED MatchAlgo: 1, 3 or 5', ERR_WARNING);
    Exit();
  end;

  GetClientDimensions(W,H);
  if (Area.X2 >= W) or (Area.X2 <= -1) then Area.X2 := W-1;
  if (Area.Y2 >= H) or (Area.Y2 <= -1) then Area.Y2 := H-1;
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;

  Screen.FromClient(Area.x1,Area.y1,Area.x2,Area.y2);
  img := __cvLoadFromMatrix(Screen.ToMatrix());
  templ := __cvLoadFromMatrix(Template.ToMatrix());   
  
  Corr := __MatchTemplate(img,templ,MatchAlgo,False);
  if (Corr.width() > 0) and (Corr.height() > 0) then
  begin
    case (MatchAlgo = TM_SQDIFF_NORMED) and True of
      True:  PT := Corr.ArgMin();
      False: PT := Corr.ArgMax();
    end;
    
    Result := Corr[PT.y][PT.x] > Similarity;  
    PT.y := PT.y + Area.x1;
    PT.x := PT.x + Area.y1;
  end;
  
  Screen.Free();
  __cvFreeMatrix(img);
  __cvFreeMatrix(templ); 
end; 





{!DOCREF} {
  @method: function se.FindTemplate(out PT:TPoint; Image, Templ:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean; overload;
  @desc:
    Search for a template `templ` in the `Image` with a tolerance, returns the point where it best matched.
    
    [params]
     PT:         Resulting point
     Image:      Image to search in.
     Templ:      Template image to search for.
     Similarity: `-1.0` to `1.0` where `+/-1.0` should be exact match.    
     MatchAlgo:  Comparison algorithm -> `TM_SQDIFF_NORMED = 1`,  `TM_CCORR_NORMED = 3`, TM_CCOEFF_NORMED = 5` 
    [/params]
}
function SimbaExt.FindTemplate(out PT:TPoint; Image, Templ:TRafBitmap; Similarity:Single; MatchAlgo: Int8 = 5): Boolean; overload;
var
  Corr:TFloatMatrix;
  patch,img:CVMat; 
begin
  if (MatchAlgo = 0) or (MatchAlgo = 2) or (MatchAlgo = 4) then begin
    RaiseWarning('se.FindTemplate only supports NORMED MatchAlgo: 1, 3 or 5', ERR_WARNING);
    Exit();
  end;
  if not((Image.Width > 0) and (Image.Height > 0)) then
    RaiseException(erOutOfRange,'Image is Empty');
    
  if not((Templ.Width > 0) and (Templ.Height > 0)) then
    RaiseException(erOutOfRange,'Templ is Empty');

  img := __cvLoadFromMatrix(Image.ToMatrix());
  patch := __cvLoadFromMatrix(Templ.ToMatrix());   
  
  Corr := __MatchTemplate(img,patch,MatchAlgo,False);
  case (MatchAlgo = TM_SQDIFF_NORMED) and True of
    True:  PT := Corr.ArgMin();
    False: PT := Corr.ArgMax();
  end;
  
  Result := Corr[PT.y][PT.x] > Similarity;  
  
  __cvFreeMatrix(img);
  __cvFreeMatrix(patch); 
end; 





{!DOCREF} {
  @method: function se.FindTemplateEx(out TPA:TPoint; Templ:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean;
  @desc:
    Search for a template image `Templ` on your screen with a tolerance, returns all the points over the given similarity.

    [params]
     TPA:        Resulting points
     Template:   Template image to search for
     Area:       A `TBox` of where to search.
     Similarity: `-1.0` to `1.0` where `+/-1.0` should be exact match.
     MatchAlgo:  Comparison algorithm -> `TM_SQDIFF_NORMED = 1`,  `TM_CCORR_NORMED = 3`, `TM_CCOEFF_NORMED = 5`
    [/params]
}
function SimbaExt.FindTemplateEx(out TPA:TPointArray; Templ:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean;
var
  W,H:Int32;
  Corr:TFloatMatrix;
  Screen:TRafBitmap;
  patch,img:CVMat;
begin
  if (MatchAlgo = 0) or (MatchAlgo = 2) or (MatchAlgo = 4) then begin
    RaiseWarning('se.FindTemplate only supports NORMED MatchAlgo: 1, 3 or 5', ERR_WARNING);
    Exit();
  end;

  GetClientDimensions(W,H);
  if (Area.X2 >= W) or (Area.X2 <= -1) then Area.X2 := W-1;
  if (Area.Y2 >= H) or (Area.Y2 <= -1) then Area.Y2 := H-1;
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;

  Screen.FromClient(Area.x1,Area.y1,Area.x2,Area.y2);
  img := __cvLoadFromMatrix(Screen.ToMatrix());
  patch := __cvLoadFromMatrix(Templ.ToMatrix());

  Corr := __MatchTemplate(img,patch,MatchAlgo,False);
  case (MatchAlgo = TM_SQDIFF_NORMED) and True of
    True:  TPA := Corr.Indices(1.0-Similarity, CMP_LTE);
    False: TPA := Corr.Indices(Similarity, CMP_GTE);
  end;

  Result := Length(TPA) > 0;
  Screen.Free();
  __cvFreeMatrix(img);
  __cvFreeMatrix(patch);
  
  if not(Result) then Exit();
  
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;



{!DOCREF} {
  @method: function se.FindTemplateEx(out TPA:TPointArray; Image, Templ:TRafBitmap; Area:TBox; Similarity:Single; MatchAlgo: Int8 = 5): Boolean; overload;
  @desc:
    Search for a template `Templ` in the `Image` with a tolerance, returns the point where it best matched.
    
    [params]
     TPA:        Resulting points
     Image:      Image to search in.
     Templ:      Template image to search for.
     Similarity: `-1.0` to `1.0` where `+/-1.0` should be exact match.    
     MatchAlgo:  Comparison algorithm -> `TM_SQDIFF_NORMED = 1`,  `TM_CCORR_NORMED = 3`, `TM_CCOEFF_NORMED = 5` 
    [/params]
    
    Example:
    [code=pascal]
    var i:Int32;
        TPA:TPointArray;
        ATPA:T2DPointArray;
        BMP,SUB:TRafBitmap;
    begin
      BMP.Open('lenas.png');
      SUB.Open('lena_sub.png');
      se.FindTemplateEx(TPA, BMP, SUB, 0.60, TM_CCOEFF_NORMED);

      //cluster to remove neighburhood-noise points
      ATPA := TPA.Cluster(3,False);

      //draw a box around each location
      for i:=0 to High(ATPA) do begin
        TPA[0] := ATPA[i].Mean();
        TPA := ToBox(TPA[0].x,  TPA[0].y,  TPA[0].x+Sub.Width,  TPA[0].y+Sub.Height).ToCoords();
        BMP.SetPixels(se.ConnectTPA(TPA), 16777215);
        BMP.SetPixels(se.TPACross(TPA[0], 5), 255);  
      end;

      BMP.Debug();
      SUB.Free();
      BMP.Free();
    end.
    [/code]
    
    Output:
    [img]http://slackworld.net/downloads/FindTemplateEx.png[/img]
}
function SimbaExt.FindTemplateEx(out TPA:TPointArray; Image, Templ:TRafBitmap; Similarity:Single; MatchAlgo: Int8 = 5): Boolean; overload;
var
  Corr:TFloatMatrix;
  patch,img:CVMat; 
begin
  if (MatchAlgo = 0) or (MatchAlgo = 2) or (MatchAlgo = 4) then begin
    RaiseWarning('se.FindTemplate only supports NORMED MatchAlgo: 1, 3 or 5', ERR_WARNING);
    Exit();
  end;

  img := __cvLoadFromMatrix(Image.ToMatrix());
  patch := __cvLoadFromMatrix(Templ.ToMatrix());   
  
  Corr := __MatchTemplate(img,patch,MatchAlgo,False);
  case (MatchAlgo = TM_SQDIFF_NORMED) and True of
    True:  TPA := Corr.Indices(1.0-Similarity, CMP_LTE);
    False: TPA := Corr.Indices(Similarity, CMP_GTE);
  end;
  
  Result := Length(TPA) > 0;
  
  __cvFreeMatrix(img);
  __cvFreeMatrix(patch); 
end;









{!DOCREF} {
  @method: function se.MatchTemplate(Image, Templ:TRafBitmap;  MatchAlgo: UInt8; Normalize:Boolean=False): TFloatMatrix;
  @desc:
    The function slides through `image`, compares the overlapped patches of size w*h against `templ` using the specified method and stores the comparison results in the `Result`.
    Exists overloads that take two matrices instead of bitmaps.
    [params]
     Image:   Image where the search is running.
     Templ:   Searched template. It must be not greater than the source image.
     MatchAlgo:  Comparison algorithm -> `TM_SQDIFF`, `TM_SQDIFF_NORMED`, `TM_CCORR`,  `TM_CCORR_NORMED`, `TM_CCOEFF`, `TM_CCOEFF_NORMED`
     Normalize:  Normalizes the result between `0.0` and `1.0`
    [/params]
}
function SimbaExt.MatchTemplate(Image, Templ:TRafBitmap;  MatchAlgo: UInt8; Normalize:Boolean=False): TFloatMatrix;
var
  W,H:Int32;
  patch,img:CVMat;
begin
  img := __cvLoadFromMatrix(Image.ToMatrix());
  patch := __cvLoadFromMatrix(Templ.ToMatrix());

  Result := __MatchTemplate(img,patch,MatchAlgo,Normalize);

  __cvFreeMatrix(img);
  __cvFreeMatrix(patch);
end;


function SimbaExt.MatchTemplate(Image, Templ:TIntMatrix;  MatchAlgo: UInt8; Normalize:Boolean=False): TFloatMatrix; overload;
var
  W,H:Int32;
  patch,img:CVMat;
begin
  img := __cvLoadFromMatrix(Image);
  patch := __cvLoadFromMatrix(Templ);

  Result := __MatchTemplate(img,patch,MatchAlgo,Normalize);

  __cvFreeMatrix(img);
  __cvFreeMatrix(patch);
end;

{$F+}
