{$DEFINE SE_MOUSE_UTILS}
type
  TMouseObj = record
    Speed:Int8;
    FOnMouseMove: TMouseMoveEvent;
  end;

const
  __MouseAction:Array of Int8 = [1,0,2];

var
  Mice: TMouseObj;

function TMouseObj.GetPosition(): TPoint;
begin
  GetMousePos(Result.x, Result.y);
end;


procedure TMouseObj.SetPosition(PT: TPoint);
begin
  if (@FOnMouseMove <> nil) then
    FOnMouseMove(@Self, [], PT.X, PT.Y);
  MoveMouse(PT.x, PT.y);
end;


procedure TMouseObj.ButtonDown(btn:TMouseButton);
var PT: TPoint;
begin
  PT := Self.GetPosition();
  HoldMouse(PT.x,PT.y,__MouseAction[UInt32(btn)]);
end;


procedure TMouseObj.ButtonUp(btn:TMouseButton);
var PT: TPoint;
begin
  PT := Self.GetPosition();
  ReleaseMouse(PT.x,PT.y,__MouseAction[UInt32(btn)]);
end;


function TMouseObj.ButtonState(btn:TMouseButton): Int32;
begin
  Result := ord(IsMouseButtonDown(__MouseAction[UInt32(btn)]));
end;


procedure TMouseObj.DragTo(PT:TPoint; btn:TMouseButton);
begin
  Self.ButtonDown(btn);
  Self.Move(PT);
  Self.ButtonUp(btn);
end;


procedure TMouseObj.DragTo(Box:TBox; btn:TMouseButton); overload;
begin
  Self.ButtonDown(btn);
  Self.Move(Box);
  Self.ButtonUp(btn);
end;


procedure TMouseObj.Scroll(x,y:Int32; Lines:UInt32);
begin
  ScrollMouse(x,y,lines);
end;


{$IFNDEF CODEINSIGHT}
(*
  Moves the mouse from current position to the given postion (xe,ye)
  
  This function is based of BenLand100`s "windMouse", and released under the GNU-GPL v3.
  https://github.com/BenLand100/SMART
  All right of this function therefore goes to Benjamin Land.
  
  Modifications made: 
  - Slows down gradually once it's close to the target.
*)
procedure TMouseObj.__mouseMove(xe,ye, Gravity, wind, meanWait, stdWait, maxStep, targetArea:Double);
var
  sqrt3,sqrt5,dist,step,randomDist,xs,ys:Double;
  veloX, veloY, veloMag, windX, windY, Base, w: Double;
  mx,my,cx,cy,i:Int32;
begin
  sqrt3 := Sqrt(3);
  sqrt5 := Sqrt(5);
  GetMousePos(cx,cy);
  xs := cx; ys := cy;
  veloX := 0; veloY := 0; windX := 0; windY := 0;
  Base := Hypot(xs - xe,ys - ye);
  repeat
    dist := Hypot(xs - xe,ys - ye);
    if dist <= 1 then Break;

    wind := Double(minE(wind, dist));
    if (dist >= targetArea) then begin
      windX := windX / sqrt3 + (Random() * (wind * 2.0 + 1.0) - wind) / sqrt5;
      windY := windY / sqrt3 + (Random() * (wind * 2.0 + 1.0) - wind) / sqrt5;
    end else begin
      windX := windX / sqrt3;
      windY := windY / sqrt3;
      if (maxStep < 3) then
          maxStep = Random() * 3 + 3.0
      else
          maxStep := maxStep / sqrt5;
    end;
    veloX := veloX + windX + gravity * (xe - xs) / dist;
    veloY := veloY + windY + gravity * (ye - ys) / dist;
    veloMag := Hypot(veloX, veloY);
    if (veloMag > maxStep) then
    begin
      randomDist := maxStep / 2.0 + Random() * maxStep / 2.0;
      veloX := (veloX / veloMag) * randomDist;
      veloY := (veloY / veloMag) * randomDist;
    end;
    xs := xs + veloX;
    ys := ys + veloY;
    mx := Round(xs);
    my := Round(ys);
    if (cx <> mx) or (cy <> my) then
      MoveMouse(mx, my);
    step := Hypot(xs - cx, ys - cy);
    if (i mod Speed = 0) then  begin
      w := se.Gauss(2.0,stdWait);
      if (dist < Base*0.1) then MaxStep := MaxE(MaxStep-0.05, 1.0);
      Wait(Abs(Round(Min(Ceil(Pow((dist+20),-1.005)*667),20)+w)));
    end;
    inc(i);
  until Hypot(xs - xe,ys - ye) <= 1;
  MoveMouse(Round(xe), Round(ye));
end;
{$endif}


procedure TMouseObj.Move(TargetX, TargetY:Int32);
var f:Double;
begin
  if Speed <= 0 then Speed := 7;
  f := se.Gauss(0.9,0.25);
  Self.__mouseMove(Targetx,Targety,8.9,3.0,3.0,0.5,2.0*f,8.0*f);
end;


procedure TMouseObj.Move(Target:TPoint); overload;
var f:Double;
begin
  if Speed <= 0 then Speed := 7;
  f := se.Gauss(0.9,0.25);
  Self.__mouseMove(Target.x,Target.y,8.9,3.0,3.0,0.5,2.0*f,8.0*f);
end;


procedure TMouseObj.Move(Target:TPoint; Rand:Int32); overload;
var
  f:Double;
  rr: TPoint;
begin
  if Speed <= 0 then Speed := 7;
  f := se.Gauss(0.9,0.25);
  rr := se.RandomPoint(Target, rand);
  Self.__mouseMove(rr.x,rr.y,8.9,3.0,3.0,0.5,2.0*f,8.0*f);
end;


procedure TMouseObj.Move(Target:TBox); overload;
var
  f:Double;
  rr: TPoint;
  W,H:Int32;
begin
  if Speed <= 0 then Speed := 7;
  f := se.Gauss(0.9,0.20);
  rr := se.RandomPoint(Target);
  Self.__mouseMove(rr.x,rr.y,8.9,3.0,3.0,0.5,2.0*f,8.0*f);
end;


procedure TMouseObj.Click(btn:TMouseButton);
begin
  Self.ButtonDown(btn);
  Wait(Round(se.Gauss(91,16)));
  Self.ButtonUp(btn);
end;


procedure TMouseObj.Click(PT:TPoint; btn:TMouseButton); overload;
begin
  Self.Move(PT);
  Self.ButtonDown(btn);
  Wait(Round(se.Gauss(91,16)));
  Self.ButtonUp(btn);
end;


procedure TMouseObj.Click(Box:TBox; btn:TMouseButton); overload;
var PT:TPoint;
begin
  pt := se.RandomPoint(Box);
  Self.Move(PT);
  Self.ButtonDown(btn);
  Wait(Round(se.Gauss(91,16)));
  Self.ButtonUp(btn);
end;


procedure TMouseObj.ClickReturn(Box:TBox; btn:TMouseButton);
var S,PT:TPoint;
begin
  S := Self.GetPosition();
  pt := se.RandomPoint(Box);
  Self.Move(PT);
  Self.ButtonDown(btn);
  Wait(Round(se.Gauss(91,16)));
  Self.ButtonUp(btn);

  Wait(Round(se.Gauss(115,26)));
  Self.Move(S);
end;


begin
  mice.FOnMouseMove := nil;
end;
