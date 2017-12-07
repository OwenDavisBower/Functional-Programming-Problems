(* 6.2.6 *)

fun count(Leaf(x), int) = if x = int then 1 else 0
  | count(Internal(x, left, right), int) = if (x = int) then 1 + count(left, int) + count(right, int)
  													  else count(left, int) + count(right, int);

(* 6.2.7 *)

fun height(Leaf(_)) = 0
  | height(Internal(_, left, right)) = if height(left) > height(right) then 1 + height(left)
  														   else 1 + height(right);

(* 6.2.8 *)

fun links(Leaf(_)) = 0
  | links(Internal(_, left, right)) = 2 + links(left) + links(right);

(* 6.2.14 *)

fun printExpression(Leaf(x)) = Int.toString(x)
  | printExpression(Internal(Plus, exp1, exp2)) = "(" ^ printExpression(exp1) ^ "+" ^ printExpression(exp2) ^ ")"
  | printExpression(Internal(Minus, exp1, exp2)) = "(" ^ printExpression(exp1) ^ "-" ^ printExpression(exp2) ^ ")"
  | printExpression(Internal(Mul, exp1, exp2)) = "(" ^ printExpression(exp1) ^ "*" ^ printExpression(exp2) ^ ")"
  | printExpression(Internal(Div, exp1, exp2)) = "(" ^ printExpression(exp1) ^ "/" ^ printExpression(exp2) ^ ")";

(* 6.2.15 *)

fun execute(Leaf(x)) = x
  | execute(Internal(Plus, exp1, exp2)) = execute(exp1) + execute(exp2)
  | execute(Internal(Minus, exp1, exp2)) = execute(exp1) - execute(exp2)
  | execute(Internal(Mul, exp1, exp2)) = execute(exp1) * execute(exp2)
  | execute(Internal(Div, exp1, exp2)) = execute(exp1) div execute(exp2);

(* 6.2.16 *)

fun numOperators(Leaf(_)) = 0
  | numOperators(Internal(_, exp1, exp2)) = 1 + numOperators(exp1) + numOperators(exp2);

(* 6.2.17 *)

fun numNumbers(Leaf(_)) = 1
  | numNumbers(Internal(_, exp1, exp2)) = numNumbers(exp1) + numNumbers(exp2);

