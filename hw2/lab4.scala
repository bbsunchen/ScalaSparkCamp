/***********************************************************
 ***********************************************************
 * CMPSC 497B/CSE 597B Lab 4 Exercises. Oct 30. Due 11/04 **
 ***********************************************************
 ***********************************************************/

 
 /*** INSTRUCTIONS ****/
 // Upload this file to ANGEL

/**** NO MUTABLE STATE (no vars or mutable data structures) ***
 **** ALL RECURSIVE FUNCTIONS MUST BE TAIL RECURSIVE (no for loops or while loops!!!) *******
 **** You may only use the following list commands: ****
 * Nil (an empty list)
 * ::  (add an element to the head of the list. for example 1::2::Nil is the list List(1,2)
 * x.head (returns the element at the head of the list)
 * x.tail (returns a List containing the elements from position 1 to the end)
 ***** YOU MAY NOT USE ANY OTHER LIST COMMANDS FOR THIS LAB
 ***** For example, no indexing such as List(23) (it is not constant time)
 ****** Do not use List(1,2,3) to create a list, do not use list.tabulate, etc.
 */
 
 /************************ NOTE **********************************/
 //Since these are code stubs, this code will not compile and so you may want to comment out
 //problems you have not finished yet. The compilation problems will result because the functions
 // have the annotation @annotation.tailrec but do not yet make any recursive calls (which you need to add)
 /************************ QUESTIONS *****************************/
 
 /* Question 1: fill in the reverse function that takes a list and outputs the reverse of the list
  * For example, reverse(List(1,2,3)) should yield List(3,2,1)
  * You are required to use tail recursion
  * you may need to modify the function to use additional arguments (that have default values)
  * If you are stuck, consider how you would use a  while loop. The mutable state in the while loop
  * corresponeds to the additional parameters your function would need */


@annotation.tailrec
def reverse[T](mylist: List[T], accumulator: List[T]=Nil): List[T] = {
	if (mylist == Nil){
		accumulator
	}
	else{
		return reverse(mylist.tail, mylist.head::accumulator)
	}
}

/*val A= List("hello", "world")
val B = reverse(A)
println(B)*/


 /* Question 2: fill in zipWithIndex. This function should take a list of Strings, such as List("hello", "world") and returns
  * a list of type List[(String, Int)] such as List(("hello", 0), ("world", 1)) where each element is a tuple of a string (from the input)
  * and its corresponding position in the input list
  *
  * Since you are required to use constant time list operations such as :: and tail, it may be easier to first generate a reversed List
  * such as List(("world", 1),("hello", 0)) and then use the reverse function to reverse this list
  * Thus you will need a tail recursive helper function to generate the initial reversed list List(("world", 1),("hello", 0))
  *
  * Hint: first consider how you would do this in a while loop where currentIndex is the current index of the List element you are working with,
  *  accumulator is your partially constructed output, and mylist is the sublist of input starting at currentIndex (hint: use the tail operation)
  */
 
def zipWithIndex(input: List[String]) = {
	@annotation.tailrec
	def helper(mylist: List[String], currentIndex: Int=0, accumulator: List[(String, Int)] = Nil): List[(String, Int)] = {
		if (mylist == Nil){
			accumulator
		}else{
			helper(mylist.tail, currentIndex+1, (mylist.head, currentIndex)::accumulator)
		}
	}
	val reversedList = helper(input)
	reverse(reversedList)
}

/*
val A= List("hello", "world", "sun", "chen")
val B = zipWithIndex(A)
println(B)
*/
 
 
 /* Question 3: this is a generalized version of Q2. The zip function should zip two lists together. 
  * For example, zip(List(2,4,7), List("hello", "to","day")) should return List((2, "hello"), (4, "to"), (7, "day"))
  * So that this works on lists of all types, we use type variables S and T instead of specifying that one list has Strings and another Ints, etc.
  * you may need to modify parameters of the helper function */
def zip[S,T](first: List[S], second: List[T]) = {
	@annotation.tailrec
	def helper(myfirst: List[S], mysecond: List[T], accumulator: List[(S,T)] = Nil): List[(S,T)] = {
		if(myfirst == Nil || mysecond == Nil){
			accumulator
		}else{
			helper(myfirst.tail, mysecond.tail, (myfirst.head, mysecond.head)::accumulator)
		}
	}
	val reversedList = helper(first,second)
	reverse(reversedList)
}

/*
val A = List(2,4,7,8,1,2,3,4,5,6,6,7)
val B = List("hello", "to","day")
val C = zip(A,B)
println(C)
*/
 
 
 /* Question 4: fill in the mapper function which squares all of the elements in its input.
  * so mapper(List(1,2,3)) should output List(1,4,9)
  * because you are restricted to using the constant-time List operations, it may be easier to first
  * generate a reversed list List(9,4,1) and then to reverse it using the reverse function in Q1.
  * Hence you will need a mapper_helper function that generates  the reversed list. So you will probably
  * need to fill in parameters and code into the mapper_helper function */

def mapper(input: List[Int]) = {
	@annotation.tailrec
	def mapper_helper(mylist: List[Int], accumulator: List[Int] = Nil): List[Int] = {
		if(mylist == Nil){
			accumulator
		}else{
			mapper_helper(mylist.tail, (mylist.head*mylist.head)::accumulator)
		}
	}
	val reversedInput = mapper_helper(input)
	reverse(reversedInput)
}

/*
val A = List(1,2,3)
val B = mapper(A)
println(B)
*/

 
 /* Question 5: the mapper function should take an input and a function (from Int to Strings)
  * and return a new list that we get by applying fn to each element. For example
  * mapper2(List(1,2,3)){x=> (x*x).toString} should return List("1","4","9")*/
 
def mapper2(input: List[Int])(fn: Int=>String): List[String] = {
    @annotation.tailrec
    def mapper_helper(mylist: List[Int], accumulator: List[String] = Nil): List[String] = {
		if(mylist == Nil){
			accumulator
		}else{
			mapper_helper(mylist.tail, fn(mylist.head)::accumulator)
		}
    }
    
    val reversedInput = mapper_helper(input)
    reverse(reversedInput)
}

/*
val A = List(1,2,3)
val B = mapper2(A){(x:Int) => (x*x).toString}
println(B)
*/

 
 /* Question 6: same as question 5, but uses type variables to make it more generic*/
 
 
def mapper3[S,T](input: List[T])(fn: T=>S): List[S] = {
    @annotation.tailrec
    def mapper_helper(mylist: List[T], accumulator: List[S] = Nil): List[S] = {
		if(mylist == Nil){
			accumulator
		}else{
			mapper_helper(mylist.tail, fn(mylist.head)::accumulator)
		}
    }
    
    val reversedInput = mapper_helper(input)
    reverse(reversedInput)
}
 
/*val A = List(1,2,3)
val B = mapper2(A){(x:Int) => (x*x).toString}
println(B)*/

 
 /* Question 7: write a function filter, which takes a list and a function and only returns those elements for which the function returns true
  * For example filter(List("this", "is", "a", "list")){x => x.contains("is")}   should return List("this", "is", "list")
  * You may need to use a helper function. Remember: only tail recursion, and not for or while loops*/
 
 def filter[T](input: List[T])(fn: T=>Boolean): List[T] ={
    @annotation.tailrec
    def filter_helper(mylist: List[T], accumulator: List[T] = Nil): List[T] = {
		if(mylist == Nil){
			accumulator
		}else{
			if(fn(mylist.head)){
				filter_helper(mylist.tail, mylist.head::accumulator)
			}
			else{
				filter_helper(mylist.tail, accumulator)
			}
		}
    }
    
    val reversedInput = filter_helper(input)
    reverse(reversedInput)
 }
 /*val A = filter(List("this", "is", "a", "list")){x => x.contains("is")}
 println(A)*/
 
 /* Question 8: write a function count, which take a list and a function and counts how many elements evaluate to true
  * For example count(List("this", "is", "a", "list")){x => x.contains("is")}   should return 3
  * You may need to add parameters with default values. Remember: only tail recursion, and not for or while loops*/

@annotation.tailrec
def count[T](mylist: List[T], num:Int = 0)(fn: T=>Boolean): Int = {
    if(mylist == Nil){
		num
	}else{
		if(fn(mylist.head)){
			count(mylist.tail, num+1)(fn)
		}else{
			count(mylist.tail, num)(fn)
		}
	}
}
/*val A = count(List("this", "is", "a", "list")){x => x.contains("is")}
println(A)*/

/* Question 9 write a tail-recursive function that returns the product of the elements in a list
 * You may need to add parameters with default values*/

@annotation.tailrec
def mult(mylist: List[Double], product:Double = 0): Double = {
    if(mylist == Nil){
		product
	}else{
		if(product == 0){
			mult(mylist.tail, mylist.head)
		}else{
			mult(mylist.tail, mylist.head * product)
		}
	}   
}
/*val A = Nil
val B = mult(A)
println(B)*/

/* Question 10: the mult function in Quesiton 9 is a simple example of a more general pattern that in functional programming is called fold
 * or reduce. mult(List(4,5,6)) is equivalent to first setting an accumulator = 1.0, then accumulator = f(accumulator, 4) 
 * (so no accumulator is 4.0), then accumulator = f(accumulator, 5) ( which is now f(4.0, 5) which is 20.0) and then
 * accumulator = f(accumulator, 6) (which is now f(20.0, 6) = `120.0)
 * if we want to multiply the elements in the list, then f multiplies its inputs. If we want to add the elements in the list, then f adds its inputs
 * if we want to convert all elements to strings and concatenate them, then the accumulator will be a String and f will concatenate its second 
 * to the accumulator: accumulator = "", accumulator = f(accumulator, 4), which gives "4", accumulator = f(accumulator, 5), which gives "45", etc.
 *
 * Instead of making the accumulator a mutable variable, we will use tail recursion to avoid mutable state.
 * the foldLeft function below will take a List of type List[S} (S is a type variable), an initial value for the accumulator which has type T (T is
 * another type variable), and a function that combines the accumulator with the current list element
 * For example, foldLeft(List(1,2,3), 0){(x,y)=> x + y*y} should return 14 (make sure you understand why)
 * */
@annotation.tailrec
def foldLeft[S,T](mylist: List[S], initial: T)(fn: (T,S)=> T): T = {
    if(mylist == Nil){
		initial
	}else{
		foldLeft(mylist.tail, fn(initial, mylist.head))(fn)
	} 
}
/*println(foldLeft(List(1,2,3), 0){(x,y)=> x + y*y})*/