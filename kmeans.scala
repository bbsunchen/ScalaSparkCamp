import scala.io.Source
import scala.util.Random
//spark will import more

def processLine(line: String) = {
	val y = line.split(",") map {x => x.toDouble } // _.toDouble
	//map get the same type, if is an array, then you get an array
	y.toVector //go sequentially, if want random access, use vector immutable, as array is mutable
}

def isOk(line: String) = {
	line != "" //here you can compare string directly
}

def readfile(filename: String) = {
	val d = Source.fromFile(filename).getLines // buffer.readline in java
	val it = d withFilter {y => isOk(y) } map {x => processLine(x)} // withFilter combine map with filter
	it.toList //store the iteration and convert to a list
}

class KClusters(k: Int, dim: Int) = {
	private val centers = Array.tabulate(k,dim){(a,b) => Random.nextDouble} //initialize, private, immutable
	
	private val clusterSum = Array.tabulate(k,dim){(a,b) => 0.0}
	private val clusterSize = Array.tabulate(k) {a => 0}
	
	private def distance(x: Seq[Double], c:Int) = {
		(x zip centers(c).foldLeft(0.0) {(acc,v) => acc + (v._1 - v._2)*(v._1 - v._2)}  //zip gives use a list of tuples
		// search more about foldLeft, and seq
		//zip is to combine element with its index
		//foldLeft operate, for 1 and 2 result a number, and do it with 3, return a number and do it with 4
	}
	
	def update(): Unit = {
		for(row <- 0 until k; col <- 0 until dim){
			centers(row)(col) = clusterSum(row)(col)/clusterSize(row)
			clusterSum(row)(col) = 0.0
		}
		for(row <- 0 until k){
			clusterSize(row) = 0
		}
	}
	
	def addPoint(x: Seq[Double]) = {
		val index = closestCenter(x)
		x.zipWithIndex.foreach(i => clusterSum(index)(i._2) = clusterSum(index)(i._2) + i._1)
		
		clusterSize(index) = clusterSize(index) + 1
		this //reference to our class
	}
	
	private def closestCenter(x: Seq[Double]) = {
		val dist = (0 until centers.length).map {y => (distance(x,y),y)} //start with an array, convert to the distance array
		val mymin = dist.min
		mymin._2
	}
	
	def print(){
		centers foreach {x => println(x.mkString(","))}
	}
}

def kmeans(filename: String = "mixture.txt", stop: Int = 20, k:Int) = {
	val data = readfile(filename)
	val myclusters = new KClusters(k, data(0).size)
	for(numiterations <- 0 until stop){
		data foldLeft(myclusters) {(acc, x) => acc.addPoint(x)}
		myclusters.update()
	}
	myclusters.print()
}