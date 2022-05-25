




(* record w/ different compound types *)
val x = {name="John", age=42};
val y = {name="Jane", age=42+32-10};
(* type of x: {name:string, age:int} *)

(* record w/ name and age *)

(* returns true if age field of the arguments are the same 
           false otherwise. *)
fun equal_age(p1:{age:int, name:string}, p2:{name:string, age:int}):bool =
  #age p1 = #age p2
    


(* records are like tuples with user-defined field names *)
val a_pair = (3+1,4+2)
val a_record = {second=4+2, first=3+1}

(* actually, tuples *are* just records with names 1, 2, ..., n *)
val another_pair = {2=5, 1=6}
val sum = (#1 a_pair + #1 another_pair, #2 a_pair + #2 another_pair)

