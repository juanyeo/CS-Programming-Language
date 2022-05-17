(* Juan Yeo 2017027265 *)

datatype pattern = Wildcard | Variable of string | UnitP
                    | ConstP of int | TupleP of pattern list
                    | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
                | Constructor of string * valu

(* My SML environment cannot call List functions *)
fun foldl (f: 'a * 'b->'b) (i: 'b) (l: 'a list): 'b =
    case l of
        [] => i
        | x::xs => foldl f (f(x, i)) xs

fun exists (f: 'a -> bool) (l: 'a list): bool =
    case l of
        [] => false
        | x::xs => if f(x) then true else exists f xs 

(* Problem 1: check_pat *)
fun extract_variables(p: pattern) =
    case p of
        Wildcard => []
        | Variable v => [v]
        | UnitP => []
        | ConstP c => []
        | TupleP ps => foldl (fn (p1,p2) => extract_variables(p1) @ p2) [] ps
        | ConstructorP(s,p) => extract_variables(p)

fun check_distinct(vs: string list) =
    case vs of
        [] => true
        | v::vs' => if exists (fn n => n = v) vs' 
                    then false 
                    else check_distinct vs'

fun check_pat(p: pattern) =
    let val vs = extract_variables(p)
    in
        check_distinct(vs)
    end

(* Problem 2: match *)
fun run_list (f: valu * pattern -> (string * valu) list option, vs: valu list, ps: pattern list) =
    case (vs, ps) of
        ([], []) => SOME []
        | ([], _) => NONE
        | (_, []) => NONE
        | (v::vs', p::ps') => let val mat = f(v, p)
                                    val chil = run_list(f, vs', ps')
                                in 
                                    case (mat, chil) of
                                        (SOME a, SOME b) => SOME (b @ a)
                                        | (NONE, _) => NONE
                                        | (_, NONE) => NONE
                                end
        
fun match(v: valu, p: pattern) =
    case p of
        Wildcard => SOME []
        | Variable vn => SOME [(vn,v)] 
        | UnitP => (case v of
                    Unit => SOME []
                    | _ => NONE)
        | ConstP vi => (case v of
                        Const i => if i = vi
                                    then SOME []
                                    else NONE
                        | _ => NONE)
        | TupleP ps => (case v of
                        Tuple(vs) => run_list(match, vs, ps)
                        | _ => NONE)
        | ConstructorP(s, p) => (case v of
                                Constructor(vcs, vp) => if s = vcs
                                                        then match(vp, p)
                                                        else NONE
                                | _ => NONE)

(* Problem 3: Implementing RPS Game *)

type name = string

datatype RSP = ROCK | SCISSORS | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament = PLAYER of name * (RSP strategy ref)
                        | MATCH of tournament * tournament

(* given functions and variables *)
fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let val Cons(rsp, func) = !strategyRef 
    in
        strategyRef := func();
        rsp 
    end

(* who's winner function *)
fun matchPlayers(p1, p2) =
    let 
        val PLAYER(n1, str1) = p1
        val PLAYER(n2, str2) = p2
        val p1next = next(str1)
        val p2next = next(str2)
    in
        case (p1next, p2next) of
            (ROCK, SCISSORS) => p1
            | (ROCK, PAPER) => p2
            | (SCISSORS, ROCK) => p2
            | (SCISSORS, PAPER) => p1
            | (PAPER, ROCK) => p1
            | (PAPER, SCISSORS) => p2
            | _ => matchPlayers(p1, p2)
    end
fun whosWinner(t: tournament) =
    case t of
        PLAYER p => t
        | MATCH (t1, t2) => 
            matchPlayers(whosWinner(t1), whosWinner(t2))


fun all_answers f lst=  
    let 
      val has_none = List.exists (fn x=> f(x) = NONE)
      val final_result = List.foldl (fn(x,acc)  => (case f(x) of SOME v => v@acc ))
    in 
    case lst of
      [] => SOME []
      |_=>if has_none lst 
          then NONE
          else SOME (final_result [] lst)
    end    
fun match (v,p)   =
     case (v,p) of  
      (_,Wildcard) => SOME []
     |(Const v1,ConstP p1) =>if v1 = p1 then SOME [] else NONE
     |(Unit,UnitP) =>SOME []
     |(Constructor (s ,v1),ConstructorP (s1, p1) ) => if s = s1 then match(v1,p1) else NONE
     |(Tuple vs,TupleP ps) => if List.length vs = List.length ps 
                              then case all_answers match (ListPair.zip(vs,ps))  of
                                    SOME v2=>SOME v2
                                   |_ => NONE
                              else NONE
     |(_, Variable s ) => SOME [(s,v)]
     |(_,_) => NONE

