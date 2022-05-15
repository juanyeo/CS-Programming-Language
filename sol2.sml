(* Juan Yeo 2017027265 *)

(* Problem 1: Simple Eval *)
datatype expr = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr

datatype formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr

fun eval_expr e =
    case e of
        NUM i => i
        | PLUS(e1, e2) => (eval_expr e1) + (eval_expr e2)
        | MINUS(e1, e2) => (eval_expr e1) - (eval_expr e2)

fun eval f =
    case f of
        TRUE => true
        | FALSE => false
        | NOT f1 => not (eval f1)
        | ANDALSO(f1, f2) => (eval f1) andalso (eval f2)
        | ORELSE(f1, f2) => (eval f1) orelse (eval f2)
        | IMPLY(f1, f2) => (not (eval f1)) orelse (eval f2)
        | LESS(e1, e2) => (eval_expr e1) < (eval_expr e2)

(* Problem 1 TEST *)
val val1 = eval(NOT(FALSE))
val val2 = eval(ANDALSO(TRUE, FALSE))
val val3 = eval(ANDALSO(TRUE, TRUE))
val val4 = eval(ORELSE(TRUE, FALSE))
val val5 = eval(ORELSE(TRUE, TRUE))
val val6 = eval(IMPLY(TRUE, TRUE))
val val7 = eval(IMPLY(TRUE, FALSE))
val val8 = eval(IMPLY(FALSE, TRUE))
val val9 = eval(IMPLY(FALSE, FALSE))
val val10 = eval(LESS(NUM 1, NUM 5))
val val11 = eval(LESS(NUM 8, NUM 5))

(* Problem 2: Check MetroMap *)
type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

fun filterName (n1, ns) =
    case ns of 
        [] => []
        | n::ns' => if n <> n1
                    then n::(filterName(n1, ns'))
                    else filterName(n1, ns')

fun connectMetro m =
    case m of
        STATION n1 => [n1]
        | AREA(n1, m1) => filterName(n1, connectMetro(m1))
        | CONNECT(m1, m2) => connectMetro(m1) @ connectMetro(m2)

fun checkMetro (m: metro) =
    let val sl = connectMetro(m)
    in
        case sl of
            [] => true
            | _ => false
    end

(* Problem 2 TEST *)
val val21 = checkMetro(AREA("a", STATION "a"))
val val22 = checkMetro(AREA("a", AREA("a", STATION "a")))
val val23 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
val val24 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))
val val25 = checkMetro(AREA("a", STATION "b"))
val val26 = checkMetro(AREA("a", AREA("a", STATION "b")))
val val27 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
val val28 = checkMetro(AREA("a", CONNECT(STATION "b", AREA("b", STATION "a"))))
val val29 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))

(* Problem 3: Lazy List *)
datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList)

fun seq (first: int, last: int) =
    if first > last then nullList
    else cons(first, fn () => seq(first + 1, last))

fun infSeq (first: int) =
    cons(first, fn() => infSeq(first + 1))

fun firstN (lazyListVal: 'a lazyList, n: int) =
    if n = 0 then []
    else
    case lazyListVal of
        nullList => []
        | cons(h, tf) => h::firstN(tf(), n - 1)

fun Nth (lazyListVal: 'a lazyList, n: int) =
    if n = 1
    then
        case lazyListVal of
            nullList => NONE
            | cons(h, tf) => SOME h
    else
        case lazyListVal of
            nullList => NONE
            | cons(h, tf) => Nth(tf(), n - 1)

fun filterMultiples(lazyListVal: int lazyList, n: int) =
    case lazyListVal of
        nullList => nullList
        | cons(h, tf) =>
            if (h mod n) = 0
            then filterMultiples(tf(), n)
            else cons(h, fn() => filterMultiples(tf(), n)) 

(* Problem 3-i TEST *)
val val31 = firstN(seq(3, 6), 7)
val val32 = firstN(infSeq(2), 15)
val val33 = Nth(infSeq(2), 15)
val val34 = firstN(filterMultiples(seq(3, 8), 3), 20)
val val35 = firstN(filterMultiples(infSeq(1), 3), 10)

fun sieve(lazyListVal: int lazyList) =
    case lazyListVal of
        cons(h, tf) => cons(h, fn() => sieve(filterMultiples(tf(), h)))

fun primes() =
    sieve(infSeq(2))

(* Problem 3-ii TEST *)
val val36 = firstN(primes(), 10)
val val37 = Nth(primes(), 20)