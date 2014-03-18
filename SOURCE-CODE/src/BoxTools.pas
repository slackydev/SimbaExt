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
  B.x1 := Modulo(B.x1, W);
  B.y1 := Modulo(B.y1, H);
  B.x2 := Modulo(B.x2, W);
  B.y2 := Modulo(B.y2, H);
end;


end.
