{!DOCTOPIC}{ 
  Random module
}

{!DOCREF} {
  @method: var Rand = TObjRandom;
  @desc: 
    This module provides you with a few Random-functions. 
    More functions will probably be introduced later.
    if SRL-6 is imported module is renamed to "Randm" :/
}


{!DOCREF} {
  @method: function Rand.Random(): Extended;
  @desc: Return the next random floating point number in the range c'0.0..1.0'. Same as c'Random();'
}
function TObjRandom.Random(): Extended;
begin
  Result := RandomE();
end;


{!DOCREF} {
  @method: function Rand.RandInt(a,b:Int32): Int32;
  @desc: Return a random integer [i]N[/i] such that c'a <= N <= b'. Same as c'RandomRange(a,b);'
}
function TObjRandom.RandInt(a,b:Int32): Int32;
begin
  Result := RandomRange(a,b);
end;


{!DOCREF} {
  @method: function Rand.Uniform(a,b:Extended): Extended;
  @desc: Return a random floating-point [i]N[/i] such that c'a <= N <= b'.
}
function TObjRandom.Uniform(a,b:Extended): Extended;
begin
  Result := a + (b-a) * Random();
end;


{!DOCREF} {
  @method: function Rand.Gauss(mu,sigma:Extended): Extended;
  @desc: Gaussian distribution. mu is the mean, and sigma is the standard deviation.
}
function TObjRandom.Gauss(mu,sigma:Extended): Extended;
var scale,theta:Extended;
begin
  Scale := sigma * Sqrt(-2 * Ln(Random()));
  Theta := 2 * PI * Random();
  Result := mu + Scale * Cos(theta);
end;


{!DOCREF} {
  @method: function Rand.TPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
  @desc: Returns a randomized TPA spread within the given bounds
}
function TObjRandom.TPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
begin
  Result := exp_RandomTPA(Amount,MinX,MinY,MaxX,MaxY);
end;


{!DOCREF} {
  @method: function Rand.CenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
  @desc:
    Returns a randomized TPA spread within the given radius, weighted towards center.
    Much faster then to do the same with some complex gaussian computation.
}
function TObjRandom.CenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
begin
  Result := exp_RandomCenterTPA(Amount,CX,CY,RadX,RadY);
end;


{!DOCREF} {
  @method: function Rand.TIA(Amount:Integer; Low,Hi:Integer): TIntArray;
  @desc: Returns a randomized TIA spread within the given bounds
}
function TObjRandom.TIA(Amount:Integer; Low,Hi:Integer): TIntArray;
begin
  Result := exp_RandomTIA(Amount,Low,Hi);
end;


{!DOCREF} {
  @method: function Rand.GaussPt(MeanPt:TPoint; Stddev:Extended): TPoint;
  @desc: Generates a gaussian ("normally" distributed) TPoint using Box-Muller transform.
}
function TObjRandom.GaussPt(MeanPt:TPoint; Stddev:Extended): TPoint;
var Theta,Scale:Extended;
begin
  Scale := Stddev * Sqrt(-2 * Ln(Random()));
  Theta := 2 * PI * Random();
  Result.x := Round(MeanPt.x + Scale * Cos(theta));
  Result.y := Round(MeanPt.y + Scale * Sin(theta));
end;


{!DOCREF} {
  @method: function Rand.GaussPt(MeanPt:TPoint; StdDev, MaxDev:Extended): TPoint; overload;
  @desc: 
    Generates a gaussian ("normally" distributed) TPoint using Box-Muller transform.
    Takes an extra parameter to encapsule the point within a given range (maxDev).
}
function TObjRandom.GaussPt(MeanPt:TPoint; StdDev, MaxDev:Extended): TPoint; overload;
var Theta,Scale:Extended;
begin
  if MaxDev < 1 then MaxDev := 1;
  Scale := Stddev * Sqrt(-2 * Ln(Random()));
  while Scale > MaxDev do
    Scale := Stddev * Sqrt(-2 * Ln(Random()));
  Theta := 2 * PI * Random();
  Result.x := Round(MeanPt.x + Scale * Cos(theta));
  Result.y := Round(MeanPt.y + Scale * Sin(theta));
end;


{!DOCREF} {
  @method: function Rand.GaussPtOval(MeanPt:TPoint; StddevX, StdDevY:Extended): TPoint;
  @desc: 
    Generates a gaussian ("normally" distributed) TPoint using Box-Muller transform.
    Takes separate x, and y parameters for Standard deviation.
}
function TObjRandom.GaussPtOval(MeanPt:TPoint; StdDevX, StdDevY:Extended): TPoint;
var ScaleX,ScaleY:Extended;
begin
  ScaleX := StdDevX * Sqrt(-2 * Ln(Random()));
  ScaleY := StdDevY * Sqrt(-2 * Ln(Random()));
  Result.x := Round(MeanPt.x + ScaleX * Cos(2 * PI * Random()));
  Result.y := Round(MeanPt.y + ScaleY * Sin(2 * PI * Random()));
end;


{!DOCREF} {
  @method: function Rand.GaussPtOval(MeanPt:TPoint; StddevX, StdDevY, MaxDevX, MaxDevY:Extended): TPoint; overload;
  @desc: 
     Generates a gaussian ("normally" distributed) TPoint using Box-Muller transform.
     Takes separate x, and y parameters for Standard deviation. 
     Takes two extra parameter to encapsule the point within a given range (maxDevX & maxDevY).
}
function TObjRandom.GaussPtOval(MeanPt:TPoint; StddevX, StdDevY, MaxDevX, MaxDevY:Extended): TPoint; overload;
var ScaleX,ScaleY:Extended;
begin
  ScaleX := StddevX * Sqrt(-2 * Ln(Random()));  
  while ScaleX > MaxDevX do
    ScaleX := StddevX * Sqrt(-2 * Ln(Random())); 
    
  ScaleY := StddevY * Sqrt(-2 * Ln(Random()));  
  while ScaleY > MaxDevY do
    ScaleY := StddevY * Sqrt(-2 * Ln(Random())); 
    
  Result.x := Round(MeanPt.x + ScaleX * Cos(2 * PI * Random()));
  Result.y := Round(MeanPt.y + ScaleY * Sin(2 * PI * Random()));
end;


{!DOCREF} {
  @method: function Rand.GaussPtDonut(MeanPt:TPoint; StdDev, MaxDev:Extended; Radius:LongInt): TPoint;
  @desc: 
    Generates a gaussian ("normally" distributed) TPoint using Box-Muller transform.
    Result in "Donut"-like shape with a gaussian distribution around the circumsphere.
}
function TObjRandom.GaussPtDonut(MeanPt:TPoint; StdDev, MaxDev:Extended; Radius:LongInt): TPoint;
var PtG, Mid:TPoint;
begin
  PtG := Self.GaussPt(MeanPt, StdDev, MaxDev);
  Mid := Self.GaussPt(MeanPt, StdDev, MaxDev);
  Result := se.ScalePoint(PtG, Mid, Radius);
end;
