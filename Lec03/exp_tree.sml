
datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp*exp
             | Multiply of exp*exp
             | If       of bool*exp*exp

(* simple example of exp tree *)
val add = Add(Constant 10, Constant ~11)
(* ==>  21  *)

(* creating exp tree *)
val ifExpr = If(false, Add(Constant 10, Constant 11),
                       Multiply(Constant 1, Constant 42))

(* true ==>  21  *)
(* false ==>  42  *)

(* evaluation of the exp tree *)
(*  eval exp -> int *)

fun eval(e: exp):int =
   case e of
     Constant(i) => 
   | Negate(e1) => 
   | Add(e1, e2) => 
   | Multiply(e1, e2) => 
   | If(b, e1, e2) => 


(* max_constant: exp -> int *)
fun max_constant (e: exp) =
  case e of
    Constant(i) => i
  | Negate e1 => 
  | Add(e1, e2) => 
  | Multiply(e1, e2) => 
  | If (_, e1, e2) => 




(*
fun max_constant2 (e: exp) =
  case e of
    Constant(i) => i
  | Negate e1 => max_constant2 (e1)
  | Add(e1, e2) => Int.max(max_constant2 e1, max_constant2 e2)
  | Multiply(e1, e2) => Int.max(max_constant2 e1, max_constant2 e2)
  | If (_, e1, e2) => Int.max(max_constant2 e1, max_constant2 e2)
*)

(*

(* count_adds: exp -> int *)
fun count_adds(e: exp) =
   case e of



*)
*)
