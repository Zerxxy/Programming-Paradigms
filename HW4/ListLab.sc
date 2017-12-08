object ListLab {
	//Problem 1++++++++++++++++++++++++++++++++++++++++++++++
	/*
		Did not know Problem 1 was done in class, so its different from
		the answers given in demos
	*/
	
	def isOdd(x: Int) = x%2 == 1              //> isOdd: (x: Int)Boolean
	def cube(x: Int) = x*x*x                  //> cube: (x: Int)Int
	
	//Iterative version
	def iterSum (list: List[Int]) = {
		var result = 0
		for (i <- list)
			if (isOdd(i)) result += cube(i)
		result
	}                                         //> iterSum: (list: List[Int])Int
	
	//Recursive version
	def reSum (list: List[Int]) : Int = {
		if (list == Nil) 0
		else if (isOdd(list.head)) cube(list.head) + reSum(list.tail)
		else reSum(list.tail)
	}                                         //> reSum: (list: List[Int])Int
	
	//Tail-Recursive version
	def tailRSum(list: List[Int]) = {
		def helper(result: Int, unused: List[Int]) : Int = {
			if (unused == Nil) result
			else if (isOdd(unused.head)) helper(result + cube(unused.head), unused.tail)
			else helper(result, unused.tail)
		}
		helper(0,list)
	}                                         //> tailRSum: (list: List[Int])Int
	
	def addCube(x: Int, y: Int) = {
		x + cube(y)
	}                                         //> addCube: (x: Int, y: Int)Int
	
	//test iter, recursive, and tail-r versions
	val list1 = List(1,2,3,4,5)               //> list1  : List[Int] = List(1, 2, 3, 4, 5)
	iterSum(list1)                            //> res0: Int = 153
	reSum(list1)                              //> res1: Int = 153
	tailRSum(list1)                           //> res2: Int = 153
	
	//Map-filter-reduce version
	def sumofCube(list: List[Int]) = {
		list.filter(isOdd _).reduce(addCube _)
	}                                         //> sumofCube: (list: List[Int])Int
	
	//test MFR version
	sumofCube(list1)                          //> res3: Int = 153
	
	
	//Problem 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	//Iterative version
	def iterSumList(list: List[List[Int]]) = {
		var result = 0
		for ( i <- list ) {
			for ( j <- i ) {
				result += j
			}
		}
		result
	}                                         //> iterSumList: (list: List[List[Int]])Int
	
	//Recursive version
	def recursiveSum (list: List[Int]) : Int = {
		if (list == Nil) 0
		else list.head + recursiveSum(list.tail)
	}                                         //> recursiveSum: (list: List[Int])Int
	
	def reSumList(list: List[List[Int]]) : Int = {
		if (list == Nil) 0
		else recursiveSum(list.head) + reSumList(list.tail)
	}                                         //> reSumList: (list: List[List[Int]])Int
	
	//Tail-Recursive version
	def tailRecursiveSum(list: List[List[Int]]) = {
		def helper(result: Int, unused: List[List[Int]]) : Int = {
			if (unused == Nil) result
			else helper(result + recursiveSum(unused.head), unused.tail)
		}
		helper(0,list)
	}                                         //> tailRecursiveSum: (list: List[List[Int]])Int
	
	val list2 = List(7,8)                     //> list2  : List[Int] = List(7, 8)
	val big = List(list1, list2)              //> big  : List[List[Int]] = List(List(1, 2, 3, 4, 5), List(7, 8))
	
	//test iter, recursive, tail-r versions
	//1+2+3+4+5+7+8 = 30
	iterSumList(big)                          //> res4: Int = 30
	reSumList(big)                            //> res5: Int = 30
	tailRecursiveSum(big)                     //> res6: Int = 30
	
	//Map-filter-reduce version
	def sum(list: List[Int]) = {
		if (list.length == 0) throw new Exception("length = 0")
    var sum = 0
    for(i <- list) sum += i
    sum
	}                                         //> sum: (list: List[Int])Int
	
	def add(x: Int,y: Int) = {
		x + y
	}                                         //> add: (x: Int, y: Int)Int
	
	def sumAll(list: List[List[Int]]) = {
		list.map(sum _).reduce(_ + _)
	}                                         //> sumAll: (list: List[List[Int]])Int
	
	//test MFR version
	sumAll(big)                               //> res7: Int = 30
	
	//Problem 3 +++++++++++++++++++++++++++++++++++++++++++++++++++++++
	def depth(value: Any): Int =
     value match {
        case first :: rest => math.max(depth(first) + 1, depth(rest))
        case _ => 0
     }                                            //> depth: (value: Any)Int
     
   val list3 = List(List(3, List(4, List(5))), List(List(5)), 6, List(7))
                                                  //> list3  : List[Any] = List(List(3, List(4, List(5))), List(List(5)), 6, List
                                                  //| (7))
   
   depth(list3)                                   //> res8: Int = 4
   
   //Problem 6 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
   //Iterative version
   def iterCount[T] (pred: T=>Boolean, list: List[T]) = {
   	var count = 0
   	for (i <- list)
   		if(pred(i)) count += 1
   	count
   }                                              //> iterCount: [T](pred: T => Boolean, list: List[T])Int
   
   def isEven(x: Int) =  x%2 == 0                 //> isEven: (x: Int)Boolean
   
   //Recursive version
   def recursiveCount[T] (pred:T=>Boolean, list: List[T]) : Int = {
   	if (list == Nil) 0
   	else if(pred(list.head)) {1 + recursiveCount(pred, list.tail)}
   	else {recursiveCount(pred,list.tail)}
   }                                              //> recursiveCount: [T](pred: T => Boolean, list: List[T])Int
   
   //test iter and recursive versions
   iterCount(isOdd _, list1)                      //> res9: Int = 3
   iterCount(isEven _, list1)                     //> res10: Int = 2
   recursiveCount(isOdd _, list1)                 //> res11: Int = 3
   recursiveCount(isEven _, list1)                //> res12: Int = 2
   
   //tail-recursive version
   def tailCount[T] (pred: T=>Boolean, list: List[T]) = {
   	def helper(result: Int, pred: T=> Boolean, unused: List[T]) : Int = {
   		if(unused == Nil) result
   		else if(pred(unused.head)) helper(result+1, pred, unused.tail)
   		else helper(result, pred, unused.tail)
   	}
   	helper(0,pred, list)
   }                                              //> tailCount: [T](pred: T => Boolean, list: List[T])Int
   
   //test tail-recursive versions
   tailCount(isOdd _, list1)                      //> res13: Int = 3
   tailCount(isEven _, list1)                     //> res14: Int = 2
   
   //map-filter-reduce version
   def count[T] (pred: T=>Boolean, list: List[T]) = {
   	list.filter(pred(_)).length
   }                                              //> count: [T](pred: T => Boolean, list: List[T])Int
   
   //test MFR version
   count(isOdd _, list1)                          //> res15: Int = 3
   count(isEven _, list1)                         //> res16: Int = 2
                               
	//Problem 7 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  def all[T](test: T=>Boolean, vals: List[T]): Boolean =
      if (vals == Nil) true
      else test(vals.head) && all(test, vals.tail)//> all: [T](test: T => Boolean, vals: List[T])Boolean
      
   def isPrime(n: Int) = {
     if (n < 2) false
     else {
        var result = true
        for(i <- 2 until n if result) {
           result = n % i != 0
        }
         result
     }
   }                                              //> isPrime: (n: Int)Boolean
   
   //test recursive versions
   all(isPrime _, List(2, 3, 5, 7, 11))           //> res17: Boolean = true
   all(isPrime _, List(2, 3, 5, 7, 9, 11))        //> res18: Boolean = false
   
   def isPal(s: String) = s == s.reverse          //> isPal: (s: String)Boolean
   
   all(isPal _, List("mom", "rotator", "dad"))    //> res19: Boolean = true
   
  // Iterative version
   def allIter[T](test: T=>Boolean, vals: List[T]) = {
      var result = true
      for(v <- vals if result) {
         println(v)
         result = result && test(v)
      }
      result
   }                                              //> allIter: [T](test: T => Boolean, vals: List[T])Boolean
   
   isPrime(4)                                     //> res20: Boolean = false
   
   //test iterative version
   allIter(isPrime _, List(2, 4, 5, 7, 11, 13, 17))
                                                  //> 2
                                                  //| 4
                                                  //| res21: Boolean = false
                                                  
   //tail-recursive version
   def tailAll [T] (pred: T=>Boolean, list: List[T]) = {
   	def helper[T](result: Boolean, unused: List[T], pred:T=>Boolean) : Boolean = {
   		if (unused == Nil) result
   		else helper(result && pred(unused.head), unused.tail, pred)
   	}
   	helper(true,list, pred)
   }                                              //> tailAll: [T](pred: T => Boolean, list: List[T])Boolean
   
   val primeList = List(2, 3, 5, 7, 11)           //> primeList  : List[Int] = List(2, 3, 5, 7, 11)
   var notPrime = List(2, 3, 5, 7, 9, 11)         //> notPrime  : List[Int] = List(2, 3, 5, 7, 9, 11)
   
   // test tail-recursive version
   tailAll(isPrime _, primeList)                  //> res22: Boolean = true
   tailAll(isPrime _, notPrime)                   //> res23: Boolean = false
   
   //map-filter-reduce version
   def allHas[T] (pred: T=>Boolean, list: List[T]) = {
   	list.map(pred(_)).reduce(_ && _)
   }                                              //> allHas: [T](pred: T => Boolean, list: List[T])Boolean
   
   //test MFR version
   allHas(isPrime _, primeList)                   //> res24: Boolean = true
   allHas(isPrime _, notPrime)                    //> res25: Boolean = false
   
   
   //Problem 8 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   // iterative version
   def containIter[T] (pred: T=> Boolean, list: List[T]) = {
   	var result = false
   	for (i <- list) {
   		if (pred(i)) result = true
   	}
   	result
   }                                              //> containIter: [T](pred: T => Boolean, list: List[T])Boolean
   
   val oneOdd = List(2,4,5,6)                     //> oneOdd  : List[Int] = List(2, 4, 5, 6)
   val noOdd = List(2,4,6,8)                      //> noOdd  : List[Int] = List(2, 4, 6, 8)
   
   // test iterative version
   containIter(isOdd _, oneOdd)                   //> res26: Boolean = true
   containIter(isOdd _, noOdd)                    //> res27: Boolean = false
   
   // recursive version
   def containRecur[T] (pred: T=>Boolean, list: List[T]) : Boolean = {
   	if (list == Nil) false
   	else pred(list.head) || containRecur(pred, list.tail)
   }                                              //> containRecur: [T](pred: T => Boolean, list: List[T])Boolean
   
   // test recursive version
   containRecur(isOdd _, oneOdd)                  //> res28: Boolean = true
   containRecur(isOdd _, noOdd)                   //> res29: Boolean = false
   
   // tail-recursive version
   def containTail[T] (pred: T=>Boolean, list: List[T]) = {
   	def helper[T]( result: Boolean, pred: T=>Boolean, unused: List[T]) : Boolean = {
   		if (unused == Nil) result
   		else helper(result || pred(unused.head), pred, unused.tail)
   	}
   	helper(false,pred,list)
   }                                              //> containTail: [T](pred: T => Boolean, list: List[T])Boolean
   
   // test tail-recursive version
   containTail(isOdd _, oneOdd)                   //> res30: Boolean = true
   containTail(isOdd _, noOdd)                    //> res31: Boolean = false
   
   //map-filter-reduce version
   def hasOne[T] (pred: T=>Boolean, list: List[T]) = {
   	list.map(pred(_)).reduce(_ || _)
   }                                              //> hasOne: [T](pred: T => Boolean, list: List[T])Boolean
   
   // test MFR version
   hasOne(isOdd _, oneOdd)                        //> res32: Boolean = true
   hasOne(isOdd _, noOdd)                         //> res33: Boolean = false
   
   //Problem 10 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   def isSorted(list: List[Int]) = {
   	var result = true
   	for(i <- 1 until list.length) {
   		if(list(i) < list(i-1)) result = false
   	}
   	result
   }                                              //> isSorted: (list: List[Int])Boolean
   
   val notSorted = List(5,4,3,2,1)                //> notSorted  : List[Int] = List(5, 4, 3, 2, 1)
   
   isSorted(list1)                                //> res34: Boolean = true
   isSorted(notSorted)                            //> res35: Boolean = false
   
   //Problem 13 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++==
   //Stream of Non-negative integers
   def makeNats(from: Int): Stream[Int] = from #:: makeNats(from + 1)
                                                  //> makeNats: (from: Int)Stream[Int]
   
   val nats = makeNats(0)                         //> nats  : Stream[Int] = Stream(0, ?)
   
   nats(5)                                        //> res36: Int = 5
   
   nats                                           //> res37: Stream[Int] = Stream(0, 1, 2, 3, 4, 5, ?)
   
   val primes = nats.filter(isPrime _)            //> primes  : scala.collection.immutable.Stream[Int] = Stream(2, ?)
   
   primes(5)                                      //> res38: Int = 13
   
   val cubedPrimes = primes.map((x: Int) => x * x * x)
                                                  //> cubedPrimes  : scala.collection.immutable.Stream[Int] = Stream(8, ?)
   
   cubedPrimes(5)                                 //> res39: Int = 2197
  
	 //One integer forever, in this case #1
   def oneStream(forever: Int): Stream[Int] = forever #:: oneStream(forever)
                                                  //> oneStream: (forever: Int)Stream[Int]
  
   val forever1 = oneStream(1)                    //> forever1  : Stream[Int] = Stream(1, ?)
   forever1(10)                                   //> res40: Int = 1
   forever1                                       //> res41: Stream[Int] = Stream(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ?)
   
   //Non-negative even integers
   val evens = nats.filter(isEven _)              //> evens  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
   evens(5)                                       //> res42: Int = 10
   evens                                          //> res43: scala.collection.immutable.Stream[Int] = Stream(0, 2, 4, 6, 8, 10, ?
                                                  //| )
   //Stream of all squared integers
   def square(i: Int) = i*i                       //> square: (i: Int)Int
   
   val squareInt = nats.map(square _)             //> squareInt  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
   squareInt(5)                                   //> res44: Int = 25
   squareInt                                      //> res45: scala.collection.immutable.Stream[Int] = Stream(0, 1, 4, 9, 16, 25, 
                                                  //| ?)
}