//===================Chen Sun(bbsunchen@outlook.com)=====================
//==================K-means algorithm on spark===========================

import org.apache.spark.rdd.RDD
import org.apache.spark.rdd.PairRDDFunctions
import org.apache.spark.SparkContext._

//====================Initialization================================
val MODULE:Int = Math.pow(2,64).toInt
val maxParameter:Int = 1000
val hashNum:Int = 32
val groupNum:Int = 3 //number of groups that hash function will be grouped for average
// Initialize random parameters for (hashNum) hash functions
val parameters = Array.tabulate(hashNum,2){
	(a,b) => scala.util.Random.nextInt(maxParameter)
}
for(i <- 0 to hashNum-1){
	if (parameters(i)(0) % 2 == 0) parameters(i)(0)= parameters(i)(0)+1
}
//====================Initialization================================

def hashFunction(i:Int, input:Int):Int={
	val value:Int = (parameters(i)(0) * input + parameters(i)(1)) % MODULE
	value
}

def findZeros(n:Int):Int={
	val s = Integer.toBinaryString(n).toList
	val reverse = s.reverse	
	var zeros = 0
	var countOne = false
	for(i <- 0 until reverse.size){
		println(reverse(i))
		if(reverse(i)=='0' && !countOne) zeros = zeros + 1 else countOne = true
	}
	zeros
}



def multiHash(x:String):List[Int]={
	val input:Int = x.toInt	
	val list:List[Int] = List.tabulate(hashNum){i=> hashFunction(i,input)}
	list
}

// for each number, find number of zeros
def findAllZeros(list:List[Int]):List[Int]={
	val zeros:List[Int] = list.map(x => findZeros(x))
	zeros
}

// find max zeros for each hash function
def findMaxZeros(a:List[Int], b:List[Int]):List[Int] ={
	val zeros:Array[Int] = a.toArray
	for(i<- 0 until a.size){
		//println(i)
		if (b(i) > a(i)) zeros(i) = b(i)
	}
	zeros.toList
}

def average(l:List[Int]):Int={
	(l.sum/l.size).toInt
}

def median(s:List[Int]):Int={
	val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
  	if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
}

def estimateResult(list:List[Int]):Int = {
	val groups = list.grouped(groupNum)
	var averageEstimates:List[Int] = Nil	
	while(groups.hasNext){
		val averageNum = average(groups.next)
		averageEstimates = averageNum::averageEstimates
	}
	median(averageEstimates)
}


// read file into sc
val infile = sc.textFile("lab5/count4").repartition(100)
//====================estimate=================================
val hashFile= infile.map(x=>multiHash(x)).map(x=>findAllZeros(x))
val maxZeros = hashFile.reduce((a,b)=>findMaxZeros(a,b))
val estimates = maxZeros.map(x=>Math.pow(2,x).toInt)
val result = estimateResult(estimates)
//==============================================================

//================exact distinguish number======================
val exact = infile.groupBy(x=>x).collect.size
//==============================================================

println("exact distinguish number is: " + exact.toString())
println("estimated number is: " + result.toString())


