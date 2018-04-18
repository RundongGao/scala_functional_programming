object e2_2 {
	def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
	  @annotation.tailrec
	  def loop(n: Int): Boolean =
	    if ((n + 1)>= as.length) true
	    else if (!ordered(as(n), as(n+1))) false
	    else loop(n +1)
	  loop(0)
	}

	def main(args: Array[String]): Unit = {
	    println("should be true", isSorted(Array(), (x:Int, y:Int) => (y > x)))
	    println("should be true", isSorted(Array(1,2,3), (x:Int, y:Int) => (y > x)))
	    println("should be false", isSorted(Array(3,2,1), (x:Int, y:Int) => (y > x)))
	    println("should be false", isSorted(Array(1,2,3), (x:Int, y:Int) => (y == x+10)))
	    println("should be true", isSorted(Array(1,11,21), (x:Int, y:Int) => (y == x+10)))
	  }
}