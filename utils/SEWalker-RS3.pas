{$IFNDEF SE_MOUSE_UTILS}
{$I SimbaExt_beta/utils/mouse.pas}
{$ENDIF}
{$f-}
(*
 Copyright (2014) Jarl <slacky> Holta
 Requires the SimbaExt version from: 8th July 2014 or later
*)
 
const
  SEW_Path = IncludePath + 'SimbaExt\utils\maps';
  SEW_Extension = '.png';
  SEW_MMCenter: TPoint = [688, 106];
  SEW_MMRad: UInt8 = 72;
  SEW_EdgeFromMid: UInt8 = 90;
 
type
  SEW_SafeProc = Procedure();
 
type
  SEWalker = record
    Map: TRafBitmap;
    Initalized:Boolean;
    ETimeOut: Int32;
    DeepScan: Boolean;
    SafeProc: SEW_SafeProc;
    SkipDist: Single;
    BlindWalk: Boolean;
    Algorithm: Int8;
    Ratio:Single;
  end;


function SEWalker.Init(MapPath:String; Method:Int8=5; MapRatio:Single=1.0): Boolean;
begin
  if (Length(MapPath) = 0) then Exit();
  if (Length(MapPath) > 3) and (MapPath[2] <> ':') then
     MapPath := SEW_Path + MapPath;

  if Ratio > 1 then Exit(False);

  Self.Map.Open(MapPath);
  Self.Map.Resize(Round(Map.Width*MapRatio), Round(Map.Height*MapRatio));
  Self.Initalized := True;
  Self.ETimeOut  := 10000;
  Self.SkipDist  := 15;
  Self.Algorithm := Method;
  Self.Ratio     := MapRatio;
  Self.BlindWalk := False;

  Result := True;
end;
 
 
(*
  Must call once you are done walking to avoid Leak.
*)
procedure SEWalker.Free();
begin
  Self.Map.Free();
end;
 
 
(*
 To avoid going in to an eternal loop for some reason,
 say if your char pings out.. this function sets the number
 of MS before the "walker" times out.
 
 > Default: 10000ms
*)
procedure SEWalker.SetTimeout(TimeMS:Int32);
begin
  Self.ETimeOut := TimeMS;
end;
 
(*
  For every step in the path we take this procedure will
  be called.. So it can call some random-check procedure, or
  just a wait procedure..
 
  > Default: nil
*)
procedure SEWalker.SetSafeProc(Proc:SEW_SafeProc);
begin
  Self.SafeProc := Proc;
end;
 
(*
  if True that means whenever the GetMyPos-function fails
  it will just click the next corrdinate in "Path" and assume
  averythings fine.
 
  > Default: False
*)
procedure SEWalker.SetBlindWalk(Allow:Boolean=False);
begin
  Self.BlindWalk := Allow;
end;
 
 
(*
  SetSkipClose sets how close to the resulting pos
  you have to be before it click the next coordinate.
 
  > Default: 5px
*)
procedure SEWalker.SetSkipClose(Dist:Single);
begin
  SkipDist := Dist;
end;
 
 
 
{---| Implementation |---------------------------------------------------------}
(*
  Get the compass angle (Radians)
  Based on a version from SRL-6 created by Olly.
*)
function SEWalker.CompassAngle(): Single;
var
  tDial: TPointArray;
  aDial: T2DPointArray;
  Angles: TExtArray;
  i,cts: Int32;
  Mid: TPoint;
  hmod,smod:Extended;
  Bools: array [0..1] of Boolean;
begin
  Mid := Point(598,34);
  cts := GetToleranceSpeed();

  GetToleranceSpeed2Modifiers(hmod,smod);
  SetColorToleranceSpeed(2);
  SetToleranceSpeed2Modifiers(2.20, 0.97);
  FindColorsTolerance(tDial, 9477483, {}582,18,614,50{}, 25);
  SetColorToleranceSpeed(cts);
  SetToleranceSpeed2Modifiers(hmod,smod);

  FilterPointsDist(tDial, 12, 15, Mid.x, Mid.y);
  if (Length(tDial) = 0) then Exit();

  aDial := SplitTPAEx(tDial, 2,2);
  SortATPAFromMidPoint(aDial, Mid);
  SetLength(tDial, 0);
  tDial := aDial[High(aDial)];

  for i := 0 to High(tDial) do
    Angles.Append( Degrees(se.Modulo(ArcTan2(tDial[i].Y - Mid.Y, tDial[i].X - Mid.X) +  PI/2, PI*2)) );

  Bools := [False, False];
  for i := 0 to High(Angles) do
    if (Angles[i] >= 0.00) and (Angles[i] <= 10.0) then
      Bools[0] := True
    else if (Angles[i] <= 360.0) and (Angles[i] >= 350.0) then
      Bools[1] := True;

  if (Bools[0]) and (Bools[1]) then
    Exit(Radians(180.0));

  Result := se.Modulo(Radians(Angles.Mean()) - Radians(180), PI*2);
end;
 
 
 
(*
  Dirty search for minimap flag (it's all that's needed)
  Based on a version from SRL-6 created by Home.
*)
function SEWalker.FlagDistance(var Dist:Single): boolean;
var
  i, bmp, l: integer;
  tpa: TPointArray;
begin
  result := False;
  bmp := BitmapFromClient(SEW_MMCenter.x - SEW_EdgeFromMid, SEW_MMCenter.y - SEW_EdgeFromMid,
                          SEW_MMCenter.x + SEW_EdgeFromMid, SEW_MMCenter.y + SEW_EdgeFromMid);

  FindColorsBitmap(bmp, tpa, 65536);
  L := Length(tpa);

  if (l < 1) then
  begin
    FreeBitmap(bmp);
    Exit();
  end;

  for i := 0 to (l - 1) do
  begin
    // to avoid out of range errors
    if (tpa[i].x < 1) then
      tpa[i].x := 1;

    if (tpa[i].y < 1) then
      tpa[i].y := 1;

    if ((FastGetPixel(bmp, tpa[i].x - 1, tpa[i].y - 1) - fastGetPixel(bmp, tpa[i].x, tpa[i].y - 1)) = 6381921) then
    begin
      Result := true;
      Dist := TPA[i].DistanceTo(SEW_MMCenter);
      Break;
    end;
  end;

  FreeBitmap(bmp);
end;


(*
 Is there any change on the minimap?
*)
function SEWalker.IsAnimating(Minshift:Int32=2000; MidWait:Int32=350): boolean;
var
  Shift:Int32;
  BMP1,BMP2:TRafBitmap;
begin
  BMP1.FromClient(580,20,790,190);  //{180,90,431,300}
  Wait(MidWait);
  BMP2.FromClient(580,20,790,190);  //{180,90,431,300}
  Shift := CalculatePixelShift(BMP1.Bitmap, BMP2.Bitmap, [0,0,BMP1.Width-1,BMP1.height-1]);
  Result := Shift > Minshift;
  BMP1.Free();
  BMP2.Free();
end;
 
(*
 Method is used to safely call the given safeproc.
*)
procedure SEWalker.CallSafeProc();
begin
  if Self.SafeProc <> nil then
    try
      Self.SafeProc();
    except
      RaiseException(erException, 'SEWalker: Failed while calling SafeProc');
    end;
end;
 
 
(*
 meh
*)
function SEWalker.GetCorrelationInfo(Mat:TFloatMatrix; out midpt:TPoint): Double;
begin
  midpt  := Mat.ArgMax();
  Result := Mat[midpt.y,midpt.x];
end;
 
 
(*
 Scans trough the whole global map for our current position.
*)
function SEWalker.GetMyPos(): TPoint;
const
  angles = [-10, 0, 10];
var
  i, size: Int32;
  BMP,RotatedBMP:TRafBitmap;
  Mid: TPoint;
  Corr: TFloatMatrix;
  values:TDoubleArray;
  PTS: TPointArray;
begin
  BMP.FromClient(SEW_MMCenter.x - SEW_MMRad, SEW_MMCenter.y - SEW_MMRad,
                 SEW_MMCenter.x + SEW_MMRad, SEW_MMCenter.y + SEW_MMRad);
 
  BMP.LazyRotate(Self.CompassAngle(), False);

  mid := Point(SEW_MMRad, SEW_MMRad);
  Size := SEW_MMRad * 2 + 1;
 
  SetLength(PTS,Length(Angles));
  for i:=0 to High(Angles) do
  begin
    RotatedBMP := BMP.Rotate(Radians(Angles[i]), False);
    RotatedBMP.LazyCrop(mid.x - SEW_MMRad, mid.y - SEW_MMRad,
                        mid.x + SEW_MMRad, mid.y + SEW_MMRad);
    if (self.ratio <> 1) then
      RotatedBMP.Resize(Round(RotatedBMP.Width  * self.ratio),
                        Round(RotatedBMP.Height * self.ratio));

    Corr := se.MatchTemplate(Map, RotatedBMP, Self.Algorithm);
    Values.Append(GetCorrelationInfo(Corr, PTS[i]));
    RotatedBMP.Free();
  end;
  Mid := PTS[Values.ArgMax()];
 
  BMP.Free();
  Mid.Offset(Point( SEW_MMRad, SEW_MMRad ));
  Result := Mid;

  //scale result according to ratio
  Result.x := Round(Result.x * 1.0/self.ratio);
  Result.y := Round(Result.y * 1.0/self.ratio);
end;
 
 
(*
 Converts the point to a point around center based around "Mid"
*)
function SEWalker.Relative(Mid,Off:TPoint): TPoint;
begin
  Result := Off;
  Result.Offset(Point(-Mid.x, -Mid.y));
end;
 
 
(*
 Gets the current pos and return the distance to the goal
*)
function SEWalker.__GetPosAndDist(Goal:TPoint; var Dist:Single): TPoint;
begin
  Result := Self.GetMyPos();
  Dist := Result.DistanceTo(Goal);
end;
 
 
(*
 Walks to the given point on the global map.
*)
function SEWalker.StepTo(Current, CurrentEst, Goal:TPoint; var Tries:Int32): Boolean;
var
  i:Int32;
  Failed:Boolean;
  Dist2: Single;
  T:Double;
begin
  if not( Initalized ) then
     RaiseException(erException, 'SEWalker: Not initalized');

  Current := Self.Relative(Current, Goal);
  Current := RotatePoint(Current, Self.CompassAngle(), 0,0);
  Current.Offset(SEW_MMCenter);

  CurrentEst := Self.Relative(CurrentEst, Goal);
  CurrentEst := RotatePoint(CurrentEst, Self.CompassAngle(), 0,0);
  CurrentEst.Offset(SEW_MMCenter);

  {if Current.DistanceTo(CurrentEst) > (SEW_EdgeFromMid / 3) then
  begin
    if Tries > 4 then
      Swap(CurrentEst, Current)
    else begin
      Inc(Tries);
      Exit(False);
    end;
  end;}

  Mice.Click(Current, mbLeft);

  T := se.MarkTime();
  while Self.FlagDistance(dist2) and (Dist2 > Self.SkipDist) do
  begin
    if (T - se.MarkTime()) > Self.ETimeOut then
      RaiseException(erException, 'SEWalker: Timed out while walking to '+
                                  '"'+ToStr(Goal)+'", from "'+ToStr(Current)+'"');
  end;
 
  for i:=1 to 3 do
    if (Dist2 > SkipDist) then Wait(100);
  Result := True;
end;
 
 
(*
 Function is used to validate a path. It ensures that each point is reachable
 from the previous point.
*)
function SEWalker.ValidatePath(Path:TPointArray): Boolean;
var i:Int32;
begin
  for i:=1 to High(Path) do
    if Path[i-1].DistanceTo(Path[i]) > SEW_EdgeFromMid then
    begin
      WriteLn('SEWalker: "Path['+ToStr(i-1)+']" to "Path['+ToStr(i)+']" is greater then MaxDist: '+ToStr(SEW_MMRad));
      Exit(False);
    end;
  Result := True;
end;
 
 
(*
 Walks a path..
*)
procedure SEWalker.Walk(Path:TPointArray);
var
  i,j,tries,hi:Int32;
  timer:Double;
  Current,CurrentEst:TPoint;
begin
  if not( Initalized ) then
     RaiseException(erException, 'SEWalker: Not initalized');
 
  if not(Self.ValidatePath(Path)) then
     RaiseException(erException, 'SEWalker: Invalid Path');

  i := 1;
  hi := High(Path);
  while i <= hi do
  begin
    Current := Self.GetMyPos();
    {$IFDEF SEW-DEBUG}Self.DebugPos(Current);{$ENDIF}
    CurrentEst := Path[i-1];
    while (i < hi) and (Current.DistanceTo(Path[i+1]) < (SEW_EdgeFromMid / 1.5)) do
      Inc(i);

    if not Self.StepTo(Current, CurrentEst, Path[i], tries) then
      Continue();
    Self.CallSafeProc();
    Inc(i);
  end;

  timer := se.MarkTime();
  for i:=1 to 3 do //tippleruplecheck! :P
    while (se.MarkTime()-timer < 3500) and Self.IsAnimating() do
      Wait(100);
end;



(*
 Debug current pos
*)
procedure SEWalker.DebugPos(pt:TPoint);
var
  bmp:TRafBitmap;
  B:TBox;
begin
  bmp := Map.Clone();
  bmp.SetPixels(se.TPACross(pt,SEW_MMRad), 255, True);
  B := [pt.x-SEW_MMRad, pt.y-SEW_MMRad, pt.x+SEW_MMRad, pt.y+SEW_MMRad];
  bmp.SetPixels(se.ConnectTPA(B.Points()), 0, True);
  bmp.Debug();
  bmp.Free();
end; 

{$f+}
