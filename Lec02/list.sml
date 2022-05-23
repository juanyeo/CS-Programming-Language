

(* sum individual elements *)
(* sum_list([1,2,3,4,5]) ==> 1+2+3+4+5 *)
(* sum_list([]) *)
fun sum_list (xs : int list) = 
  if null(xs)
  then 0
  else hd(xs) + sum_list(tl(xs))


(* x=5  [5,4,3,2,1,0] *)
(* x=0  [0] *)
fun countdown (x : int) = 




(* x:[1,2] y:[3,4,5,6] => [1,2,3,4,5,6] *)
(* x:[2] y:[3,4,5,6] => [2,3,4,5,6] *)
(* x:[] y:[3,4,5,6] => [3,4,5,6] *)
fun append (xs : int list, ys : int list) = 


