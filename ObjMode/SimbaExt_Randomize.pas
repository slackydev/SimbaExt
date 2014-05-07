{!DOCTOPIC}{ 
  Random module
}

{!DOCREF} {
  @method: function Rand.TPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
  @desc: Returns a randomized TPA spread within the given bounds
}
function TObjRandom.TPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
begin
  exp_RandomTPA(Amount,MinX,MinY,MaxX,MaxY, Result);
end;


{!DOCREF} {
  @method: function Rand.CenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
  @desc: Returns a randomized TPA spread within the given radius, weighted towards center
}
function TObjRandom.CenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
begin
  exp_RandomCenterTPA(Amount,CX,CY,RadX,RadY, Result);
end;


{!DOCREF} {
  @method: function Rand.TIA(Amount:Integer; Low,Hi:Integer): TIntArray;
  @desc: Returns a randomized TIA spread within the given bounds
}
function TObjRandom.TIA(Amount:Integer; Low,Hi:Integer): TIntArray;
begin
  exp_RandomTIA(Amount,Low,Hi, Result);
end;
