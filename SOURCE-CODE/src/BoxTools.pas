Unit BoxTools;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface

uses CoreTypes;

procedure WrapAroundBox(var B: TBox; W,H: Integer); Inline;

 
//--------------------------------------------------
implementation
uses CoreMath;

procedure WrapAroundBox(var B: TBox; W,H: Integer); Inline;
begin
  B.x1 := IModulo(B.x1, W);
  B.y1 := IModulo(B.y1, H);
  B.x2 := IModulo(B.x2, W);
  B.y2 := IModulo(B.y2, H);
end;


end.
