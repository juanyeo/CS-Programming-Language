
(* multiply x and y *)
fun mult(x:int, y:int) =
    x*y

(* apply f to x and y *)
(* higher-order function *)
fun apply_f(f:int*int -> int, x:int, y:int) =
  f(x, y)

apply_f(mult, 10, 11);



(* higher-order function *)
fun ret_f():int*int->int =
   mult

ret_f()(4, 5)


