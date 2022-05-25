
fun fact(n:int) =
    if n=0
    then 1
    else n*fact(n-1)

fun fact(0) = 1
  | fact(n) = n*fact(n-1)
