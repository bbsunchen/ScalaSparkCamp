@annotation.tailrec
def findit(arr: Seq[String], index: Int = 0)(cond: String => Boolean): Int = {
   if(index < 0 || index >= arr.size)  -1
   else if(cond(arr(index))) index
   else findit(arr, index + 1)(cond)
   
 }