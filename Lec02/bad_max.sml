



(* better_max([]) => NONE <<< 
   better_max([1,2,3])) => SOME(3)
   better_max([~1,~2,0, ~5])) => SOME(0) <<<
 *)
fun better_max (xs : int list):int option = 
    if null(xs)
    then NONE
    else 
        let val recur_sol_opt = better_max(tl xs)
        in
          if isSome(recur_sol_opt) andalso
                valOf(recur_sol_opt) > hd xs
          then recur_sol_opt
          else SOME(hd xs)
        end

val nums1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28]
val nums2 = [28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

val nums3 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]


fun better_max (xs : int list):int option = 
    if null(xs)
    then NONE
    else 
        if null (tl xs)  (* if xs has a single value *)
        then SOME(hd xs)      (* return that value *)
        else 
            let val recur_sol = better_max (tl xs) (* always returns SOME *)
            in
              if hd xs > valOf(recur_sol)
              then SOME(hd xs)
              else recur_sol
            end






(*
fun good_max (xs : int list) = 
    if null xs
    then 0 (* horrible style; fix later *)
    else 
        if null (tl xs) 
        then hd xs
        else 
           (* fill this branch *)
*)


(*
fun good_max (xs : int list) = 
  if null xs
  then 0
  else if null(tl xs)
  then hd(xs)
  else
    let val sub_ans...
    in
    end

*)

(*







fun better_max (xs : int list) = 
    if null xs
    then NONE
    else 
        let val max_rest = better_max(tl xs)
        in
            if isSome(max_rest) andalso valOf(max_rest) > hd xs
            then max_rest
            else SOME (hd xs)
        end










fun good_max (xs : int list) = 
    if null xs
    then NONE
    else 
        let fun max_nonempty(xs: int list) =
            if null (tl sx)
            then hd xs
            else
                let val max_rest = max_nonempty(tl xs)
                in
                    if hd xs > max_rest
                    then hd xs
                    else max_rest
                end
        in
            SOME max_nonempty(xs)
        end

val x = 42;

val x = 30;
 
*)
