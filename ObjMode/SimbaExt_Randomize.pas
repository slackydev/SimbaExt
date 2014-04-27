{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function TObjRandom.TPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
begin
  exp_RandomTPA(Amount,MinX,MinY,MaxX,MaxY, Result);
end;


function TObjRandom.CenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
begin
  exp_RandomCenterTPA(Amount,CX,CY,RadX,RadY, Result);
end;


function TObjRandom.TIA(Amount:Integer; Low,Hi:Integer): TIntegerArray;
begin
  exp_RandomTIA(Amount,Low,Hi, Result);
end;
