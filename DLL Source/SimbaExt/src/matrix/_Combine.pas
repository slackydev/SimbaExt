{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{*
 Combines two matrices, by the given operator
*}
function CombineMatrix(Mat1:T2DByteArray; Mat2:T2DByteArray; OP:Char): T2DByteArray; overload;
begin
  if not(length(mat1) = length(mat2)) or (length(mat1) = 0) then begin
    WriteLn('Matrices must have the same height, and not empty'); Exit();
  end;
  if not(op in ['*','/','+','-']) then begin
    WriteLn('Undefined operator: "'+op+'", expected any of "*", "/", "+" or "-"'); Exit();
  end;
  if not(length(mat1[0]) = length(mat2[0])) then begin
    WriteLn('Matrices must have the same width!'); Exit();
  end;
  
  case OP of
    '*': Result := Mat1*Mat2;
    '/': Result := Mat1/Mat2;
    '+': Result := Mat1+Mat2;
    '-': Result := Mat1-Mat2;
  end;
end;


function CombineMatrix(Mat1:T2DIntArray; Mat2:T2DIntArray; OP:Char): T2DIntArray; overload;
begin
  if not(length(mat1) = length(mat2)) or (length(mat1) = 0) then begin
    WriteLn('Matrices must have the same height, and not empty'); Exit();
  end;
  if not(op in ['*','/','+','-']) then begin
    WriteLn('Undefined operator: "'+op+'", expected any of "*", "/", "+" or "-"'); Exit();
  end;
  if not(length(mat1[0]) = length(mat2[0])) then begin
    WriteLn('Matrices must have the same width!'); Exit();
  end;
  
  case OP of
    '*': Result := Mat1*Mat2;
    '/': Result := Mat1/Mat2;
    '+': Result := Mat1+Mat2;
    '-': Result := Mat1-Mat2;
  end;
end;


function CombineMatrix(Mat1:T2DFloatArray; Mat2:T2DFloatArray; OP:Char): T2DFloatArray; overload;
begin
  if not(length(mat1) = length(mat2)) or (length(mat1) = 0) then begin
    WriteLn('Matrices must have the same height, and not empty'); Exit();
  end;
  if not(op in ['*','/','+','-']) then begin
    WriteLn('Undefined operator: "'+op+'", expected any of "*", "/", "+" or "-"'); Exit();
  end;
  if not(length(mat1[0]) = length(mat2[0])) then begin
    WriteLn('Matrices must have the same width!'); Exit();
  end;
  
  case OP of
    '*': Result := Mat1*Mat2;
    '/': Result := Mat1/Mat2;
    '+': Result := Mat1+Mat2;
    '-': Result := Mat1-Mat2;
  end;
end;


function CombineMatrix(Mat1:T2DDoubleArray; Mat2:T2DDoubleArray; OP:Char): T2DDoubleArray; overload;
begin
  if not(length(mat1) = length(mat2)) or (length(mat1) = 0) then begin
    WriteLn('Matrices must have the same height, and not empty'); Exit();
  end;
  if not(op in ['*','/','+','-']) then begin
    WriteLn('Undefined operator: "'+op+'", expected any of "*", "/", "+" or "-"'); Exit();
  end;
  if not(length(mat1[0]) = length(mat2[0])) then begin
    WriteLn('Matrices must have the same width!'); Exit();
  end;
  
  case OP of
    '*': Result := Mat1*Mat2;
    '/': Result := Mat1/Mat2;
    '+': Result := Mat1+Mat2;
    '-': Result := Mat1-Mat2;
  end;
end;


function CombineMatrix(Mat1:T2DExtArray; Mat2:T2DExtArray; OP:Char): T2DExtArray; overload;
begin
  if not(length(mat1) = length(mat2)) or (length(mat1) = 0) then begin
    WriteLn('Matrices must have the same height, and not empty'); Exit();
  end;
  if not(op in ['*','/','+','-']) then begin
    WriteLn('Undefined operator: "'+op+'", expected any of "*", "/", "+" or "-"'); Exit();
  end;
  if not(length(mat1[0]) = length(mat2[0])) then begin
    WriteLn('Matrices must have the same width!'); Exit();
  end;
  
  case OP of
    '*': Result := Mat1*Mat2;
    '/': Result := Mat1/Mat2;
    '+': Result := Mat1+Mat2;
    '-': Result := Mat1-Mat2;
  end;
end;



