{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Drawing functionality for TRafBitmap
 - Work in progress
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{!DOCTOPIC}{ 
  TRafDraw module
}

procedure TRafDraw.Create(var Image:TRafBitmap);
begin
  Self.BMP := @Image;
  Self.Loaded := True;
end;


procedure TRafDraw.Free(var Image:TRafBitmap);
begin
  Self.BMP := nil;
  Self.Loaded := False;
end;


procedure TRafDraw.Line(p2, p1:TPoint; color:Int32; AntiAliased:Boolean=True);
var
  dx,dy,step,I,H: Integer;
  rx,ry,x,y: Extended;
  r:Single;
const
  PiDiv2 = PI/2;
begin
  if not(Self.Loaded) then Exit();

  if Int64(p1) = Int64(p2) then begin
    Self.BMP^.Pixel(p1.x,p1.y,color);
    Exit;
  end;

  dx := (P2.x - P1.x);
  dy := (P2.y - P1.y);
  if (Abs(dx) > Abs(dy)) then step := Abs(dx)
  else step := Abs(dy);

  rx := dx / step;
  ry := dy / step;
  x := P1.x;
  y := P1.y;

  Self.BMP^.Pixel(p1.x,p1.y,color);
  
  if AntiAliased then
  begin
    r := Degrees(se.Modulo(ArcTan2(-(p2.y-p1.y), p2.x-p1.x)-pidiv2,PI));
    if (r > 45) and (r < 135) then begin
      for i:=1 to step do begin
        x := x + rx;
        y := y + ry;
        Self.BMP^.Pixel(Round(x),Ceil(y), color,     frac(y));
        Self.BMP^.Pixel(Round(x),Trunc(y),color, 1 - frac(y));
      end
    end else if (trunc(r) <> 45) and (trunc(r) <> 135) then begin
      for i:=1 to step do begin
        x := x + rx;
        y := y + ry;
        Self.BMP^.Pixel(Ceil(x),Round(y), color,     frac(x));
        Self.BMP^.Pixel(Trunc(x),Round(y),color, 1 - frac(x));
      end;
    end else if (trunc(r) = 45) then begin
      for i:=1 to step do begin
        x := x + rx;
        y := y + ry;
        Self.BMP^.Pixel(Round(x),Round(y),color);
        Self.BMP^.Pixel(Round(x),Round(y)-1, color, 0.25);
        Self.BMP^.Pixel(Round(x),Round(y)+1, color, 0.25);
      end;
    end else if (trunc(r) = 135) then
      for i:=1 to step do begin
        x := x + rx;
        y := y + ry;
        Self.BMP^.Pixel(Round(x),Round(y),color);
        Self.BMP^.Pixel(Round(x)-1,Round(y), color, 0.25);
        Self.BMP^.Pixel(Round(x)+1,Round(y), color, 0.25);
      end;
  end else
    for i:=1 to step do
    begin
      x := x + rx;
      y := y + ry;
      Self.BMP^.Pixel(Round(x),Round(y),color);
    end;
end; 



procedure TRafDraw.Circle(Center:TPoint; Radius:Int32; color:Int32; AntiAliased:Boolean=True);
var
  x,y:Single;
  step,theta,a:Single;
const
  PI2 = 2*PI;
  
  function D(p,q:TPoint; radius:Int32):Single; 
  begin
    Result := 1 - MinE( Abs( p.DistanceTo(q)-Radius ), 1.0);
  end;
begin
  if not(Self.Loaded) then Exit();

  step := PI2/(PI*Radius*2);

  theta:=0.0;
  while theta < PI2 do
  begin
    x := Center.x + Radius * cos(theta);
    y := Center.y - Radius * sin(theta);
    if AntiAliased then
      if ((theta > 0.78539) and (theta <= 2.35619)) or ((theta > 3.92699) and (theta <= 5.49778)) then
      begin
        Self.BMP^.Pixel(Round(x),Ceil(y), color, D(Center,[Round(x),Ceil(y)], radius));
        Self.BMP^.Pixel(Round(x),Trunc(y),color, D(Center,[Round(x),Trunc(y)],radius));
      end else
      begin
        Self.BMP^.Pixel(Ceil(x), Round(y),color, D(Center,[Ceil(x), Round(y)],radius));
        Self.BMP^.Pixel(Trunc(x),Round(y),color, D(Center,[Trunc(x),Round(y)],radius));
      end
    else
      Self.BMP^.SetPixels(se.TPACircle(center,radius,false),color, False);
      
    theta := theta + step;
  end;
  Self.BMP^.Pixel(Round(x),Ceil(y), color);
  Self.BMP^.Pixel(Round(x),Trunc(y),color);
end;



procedure TRafDraw.Polygon(Center:TPoint; Sides:Int32; Start:TPoint; Color:Int32; AntiAliased:Boolean=True);
var
  i:Int32;
  TPA:TPointArray;
begin
  if not(Self.Loaded) then Exit();
  
  TPA := SE.XagonPoints(Center,Sides,Start);
  for i:=0 to High(TPA) do
    Self.Line(TPA[i], TPA[se.modulo(i+1,length(tpa))], color, AntiAliased);
end;



procedure TRafDraw.Lines(TPA:TPointArray; Color:Int32; Connected:Boolean=False; AntiAliased:Boolean=True);
var i:Int32;
begin
  if not(Self.Loaded) then Exit();
  if connected then begin
    for i:=0 to High(TPA) do
      Self.Line(TPA[i], TPA[se.modulo(i+1,length(tpa))], Color, AntiAliased);
  end else
    for i:=0 to High(TPA)-1 do
      Self.Line(TPA[i], TPA[i+1], Color, AntiAliased);
end;


