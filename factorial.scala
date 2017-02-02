//factorial function
// f(0) = 1
// f(1) = 1
// f(2) = 2
// f(3) = 6
// f(n) = n * f(n-1)
@annotation.tailrec
def f(x: Int, accumulator: Int = 1): Int = {
    if(x == 0) accumulator
    else f(x-1, x * accumulator)
}