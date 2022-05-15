
(* (a, b) ==> (b, a) *)
fun swap (pr:int*bool): bool*int =
  (#2 pr, #1 pr)

(* (a, b), (c, d) ==> (a+c, b+d) *)
fun sum_two_pairs(p1:int*int, p2:int*int): int*int =
  (#1 p1 + #1 p2, #2 p1 + #2 p2) 

(* (a, b) ==> (a/b, a mod b) *)
fun div_mod(pr:int*int): int*int = 
  (#1 pr div #2 pr, #1 pr mod #2 pr  )