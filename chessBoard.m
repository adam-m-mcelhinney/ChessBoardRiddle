(* ::Package:: *)

(* Parameters *)
size = 8;
givenVal = 10;
(* Corners *)
bottemLeftCornerEq[{i_, j_}] := StringJoin["(c", ToString[i+1], ToString[j], " + c", ToString[i], ToString[j+1], ")/2 == c", ToString[i], ToString[j]]
topRightCornerEq[{i_, j_}] := StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j-1], ")/2 == c", ToString[i], ToString[j]]
topLeftCornerEq[{i_, j_}] := StringJoin["(c", ToString[i+1], ToString[j], " + c", ToString[i], ToString[j-1], ")/2 == c", ToString[i], ToString[j]]
bottmRightCornerEq[{i_, j_}] := StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j+1], ")/2 == c", ToString[i], ToString[j]]
(* Edges *)
bottomEdge[{i_, j_}]:= StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j+1], " + c", ToString[i+1], ToString[j], ")/3 == c", ToString[i], ToString[j]]
topEdge[{i_, j_}]:= StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j-1], " + c", ToString[i+1], ToString[j], ")/3 == c", ToString[i], ToString[j]]
leftEdge[{i_, j_}]:= StringJoin["(c", ToString[i], ToString[j+1], " + c", ToString[i+1], ToString[j], " + c", ToString[i], ToString[j-1], ")/3 == c", ToString[i], ToString[j]]
rightEdge[{i_, j_}]:= StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j+1], " + c", ToString[i], ToString[j-1], ")/3 == c", ToString[i], ToString[j]]
(* Middle *)
middleEq[{i_, j_}] := StringJoin["(c", ToString[i-1], ToString[j], " + c", ToString[i], ToString[j-1], " + c", ToString[i+1], ToString[j]," + c", ToString[i+1], ToString[j+1], ")/4 == c", ToString[i], ToString[j]]


equationSelector[{i_, j_}] := Which[ i == 1 && j == 1
							, bottemLeftCornerEq[{i, j}]
							, i == size && j == 1
							, bottmRightCornerEq[{i, j}]
							, i == size && j == size
							, topRightCornerEq[{i, j}]
							, i == 1 && j == size
							, topLeftCornerEq[{i, j}]
							, i == 1
							, leftEdge[{i, j}] 
							, i == size
							, rightEdge[{i, j}]
							, j == 1
							, bottomEdge[{i, j}]
							, j == size
							, topEdge[{i, j}]
							, i != 1 && i != size && j != 1 && j != size
							, middleEq[{i, j}]
							]



(* Test All Cases
i = 1; j = 1;
bottemLeftCornerEq[{i, j}] \[Equal] equationSelector[{i, j}]
i = 3; j = 1;
bottmRightCornerEq[{i, j}] \[Equal] equationSelector[{i, j}]
i = 1; j = 3; 
topLeftCornerEq[{i, j}] \[Equal] equationSelector[{i, j}]
i = 3; j = 3; 
topRightCornerEq[{i, j}] \[Equal] equationSelector[{i, j}]
i = 2; j = 1; 
bottomEdge[{i, j}] \[Equal] equationSelector[{i, j}]
i = 2; j = 3; 
topEdge[{i, j}] \[Equal] equationSelector[{i, j}]
i = 3; j = 2; 
rightEdge[{i, j}] \[Equal] equationSelector[{i, j}]
i = 1; j = 2; 
leftEdge[{i, j}] \[Equal] equationSelector[{i, j}]
i = 2; j = 2; 
middleEq[{i, j}] \[Equal] equationSelector[{i, j}]
 *)
(* Test on simple 2x2 case 
eq1 = bottemLeftCornerEq[{1, 1}]
eq2 = bottmRightCornerEq[{2, 1}]
eq3 = topRightCornerEq[{2, 2}]
eq4 = topLeftCornerEq[{1, 2}]
Solve[ToExpression[eq1] && ToExpression[eq2] && ToExpression[eq3]  && ToExpression[eq4] && c11 \[Equal] 10, {c11, c12, c21, c22}]
*)

(* Create System *)

(* Iterate over range *)
f[x_, y_] := {x, y}
genVar[{i_, j_}] := StringJoin["c", ToString[i], ToString[j]];
varList = Map[genVar, grid];
grid = Flatten[Outer[f, Range[size], Range[size]], 1];
eqPart1 = StringReplace[ToString[Map[equationSelector, grid]], {"{" -> "", "}" -> "", "," -> " &&"}];
SolveStr = StringJoin[eqPart1, " && c11 ==", ToString[givenVal]]

Solve[ToExpression[SolveStr], ToExpression[varList]]

(* Solve without a given value 
*)

Solve[ToExpression[eqPart1], ToExpression[varList]]

(*
Solve[(c21 + c12)/2 == c11 && (c22 + c11)/2 == c12 && (c11 + c22)/2 == c21 && (c12 + c21)/2 == c22 && c11\[Equal]10, {c11,c12,c21,c22}] *)

(*
*)
(*
startCell = c11
c11 = 10
bottemLeftCornerEq[1, 1]
topRightCornerEq[8, 8]
topLeftCornerEq[1, 8]
bottmRightCornerEq[8, 1]
bottomEdge[{2,1}]
topEdge[4,8]
leftEdge[1, 4]
rightEdge[8, 4]
Map[bottomEdge, {{2,1}}]
eq1
eq2 *)



