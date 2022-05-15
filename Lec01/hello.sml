(* This is a comment. 
*  Multi-line comment is allowed.
*  (* nested comments are also possible *)
* *)

val x = 43;

val y = 30;

val z = (x+y) * (x-y);

val abs_of_z = if z > 0 then z else 0-z;

val abs_of_z2 = abs(z);

