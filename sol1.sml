(* Juan Yeo 2017027265 *)

(* Problem 1 *)
fun merge (xs: int list, ys: int list) =
    if null xs then ys
    else if null ys then xs
    else if hd(xs) > hd(ys) then hd(ys) :: merge(xs, tl(ys))
    else hd(xs) :: merge(tl(xs), ys)

val r1 = merge([1, 4, 5], [2, 6, 7])

(* Problem 2 *)
fun reverse (xs: int list) =
    if null xs then []
    else reverse(tl(xs)) @ [hd(xs)]

val r2 = reverse([1, 5, 4])

(* Problem 3 *)
fun pi (a: int, b: int, f: int -> int) =
    if a > b then 1
    else f(a) * pi(a + 1, b, f)

val r3 = pi(2, 4, fn x => x+1)

(* Problem 4 *)
fun digits (x: int) =
    if x < 10 then [x]
    else digits(x div 10) @ [x mod 10]

val r4 = digits(253)

(* Problem 5 *)
fun addDigits (n: int) =
    if n < 10 then n
    else (n mod 10) + addDigits(n div 10)

fun additivePersistence (n: int) =
    if n < 10 then 0
    else additivePersistence(addDigits(n)) + 1

fun digitalRoot (n: int) =
    if n < 10 then n
    else digitalRoot(addDigits(n))

val r51 = additivePersistence(12349)
val r52 = digitalRoot(12349)