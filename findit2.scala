//return first index of a string that ends with ".scala", otherwise -1
//find index of a match starting from index, or return -1
@annotation.tailrec
def findit(arr: Seq[String], index: Int = 0): Int = {
   if(index < 0 || index >= arr.size)  -1
   else if(arr(index).endsWith(".scala")) index
   else findit(arr, index + 1)
   
 }
 
 //alternative with nested functions
 
 def findit2(arr: Seq[String]) = {
   val ending = ".scala"
    @annotation.tailrec
   def findit_helper(index: Int): Int = {
        if(index < 0 || index >= arr.size)  -1
        else if(arr(index).endsWith(ending)) index
        else findit(index + 1)
   
    }
 
    findit_helper(arr,0)
 
 }