fun countup(from : int, to : int) = (* for testing our max functions *)
    if from=to
    then to::[]
    else from :: countup(from+1,to)

fun countdown(from : int, to : int) = (* for testing our max functions *)
    if from=to
    then to::[]
    else from :: countdown(from-1,to)

(* better: returns an int option *)
fun max1 (xs : int list) =
    if null xs
    then 
    else 
      let val tl_ans = max1(tl xs)
      in 
        if 
        then 
        else 
      end



(* looks the same as max1 to clients; 
   implementation avoids valOf *)
fun max2 (xs : int list) =
    if null xs
    then NONE
    else let (* fine to assume argument nonempty because it is local *)
	fun max_nonempty (xs : int list) =
		if null (tl xs) (* xs better not be [] *)
		then hd xs
		else let val tl_ans = max_nonempty(tl xs)
		     in
			 if hd xs > tl_ans
			 then hd xs
			 else tl_ans
		     end
	in
	    SOME (max_nonempty xs)
	end


(* does not matter if returns an alias *)
fun sort_pair (pr : int*int) =
    if (#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr) 

 (* introduces sharing; don't care *)
fun append (xs : int list, ys : int list) = 
    if null xs
    then ys
    else hd(xs) :: append(tl(xs), ys)

