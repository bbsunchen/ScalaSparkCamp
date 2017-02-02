@annotation.tailrec
def findit[X](arr: Seq[X], index: Int = 0)(cond: X => Boolean): Int = {
   if(index < 0 || index >= arr.size)  -1
   else if(cond(arr(index))) index
   else findit(arr, index + 1)(cond)
   
 }