{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function XT_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;
begin
  exp_RandomTPA(Amount,MinX,MinY,MaxX,MaxY, Result);
end;


function XT_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; 
begin
  exp_RandomTPA(Amount,CX,CY,RadX,RadY, Result);
end;


function XT_RandomTIA(Amount:Integer; Low,Hi:Integer): TIntegerArray;
begin
  exp_RandomTIA(Amount,Low,Hi, Result);
end;
