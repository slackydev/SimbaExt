{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function se_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
begin
  exp_RandomTPA(Amount,MinX,MinY,MaxX,MaxY, Result);
end;


function se_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
begin
  exp_RandomCenterTPA(Amount,CX,CY,RadX,RadY, Result);
end;


function se_RandomTIA(Amount:Integer; Low,Hi:Integer): TIntegerArray;
begin
  exp_RandomTIA(Amount,Low,Hi, Result);
end;
