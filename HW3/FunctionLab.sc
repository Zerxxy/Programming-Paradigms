object FunctionLab {
	
	//Problem 1+++++++++++++++++++++++++++++++++++++++++++++++++++
	def compose[A,B,C] (f: B=>C, g:A=>B) = {
		def r(x: A) = f(g(x))
		r _
	}                                         //> compose: [A, B, C](f: B => C, g: A => B)A => C
	
	def f(x: Int) = x*x                       //> f: (x: Int)Int
	def g(x: Int) = x/2                       //> g: (x: Int)Int
	
	val squareOfHalf = compose(f _, g _)      //> squareOfHalf  : Int => Int = FunctionLab$$$Lambda$10/1164175787@2ff4f00f
	
	squareOfHalf(6)                           //> res0: Int = 9
	squareOfHalf(10)                          //> res1: Int = 25
	squareOfHalf(12)                          //> res2: Int = 36
	
	//Problem 2++++++++++++++++++++++++++++++++++++++++++++++++++++
	//helper functions
	def dec(n: Int) = n - 1                   //> dec: (n: Int)Int
	def isZero(n: Int) = n==0                 //> isZero: (n: Int)Boolean
	
	def selfIter[T] (f: T=>T, n: Int) = {
		def start = f
		
		def helper(function: T=>T, count: Int) : T=>T = {
			if (isZero(count-1)) function
			else helper(compose(function, f), dec(count))
		}
		
		helper(f, n)
	}                                         //> selfIter: [T](f: T => T, n: Int)T => T
	
	def inc(n: Int) = n+1                     //> inc: (n: Int)Int
	def double(n: Int) = n*2                  //> double: (n: Int)Int
	
	val inc_5 = selfIter(inc _, 5)(5)         //> inc_5  : Int = 10
	val double_5 = selfIter(double _, 5)(2)   //> double_5  : Int = 64
	val double_7 = selfIter(double _,7)(2)    //> double_7  : Int = 256
	
	//Problem 3++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	def testInt[T] (x: T) = {
		x match {
			case x:Int => true
			case _ => false
		}
	}                                         //> testInt: [T](x: T)Boolean
	def countPass[T](array: Array[T]) = {
		var count = 0
		for(index <- array) {
			if (testInt(index))
				count = count + 1
		}
		count
	}                                         //> countPass: [T](array: Array[T])Int
	
	val array1 = Array("hi", 123, "world","no", 5, 7)
                                                  //> array1  : Array[Any] = Array(hi, 123, world, no, 5, 7)
	val array2 = Array(1,2,3,4,5, "HW3")      //> array2  : Array[Any] = Array(1, 2, 3, 4, 5, HW3)
	
	countPass(array1)                         //> res3: Int = 3
	countPass(array2)                         //> res4: Int = 5
	
	
	//Problem 4++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//A
	//recursive combinator
	def recur(baseVal: Int, combiner:(Int,Int)=>Int) : Int=>Int = {
		def helper(baseVal: Int): Int = {
			if(isZero(baseVal)) 1
			else combiner(baseVal, helper(dec(baseVal)))
		}
		helper _
	}                                         //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int
	
	//B
	//factorial
	def factCombiner(a: Int, b: Int) = {
		def helper(a: Int, b: Int) : Int = {
			a * b
		}
		helper _
	}                                         //> factCombiner: (a: Int, b: Int)(Int, Int) => Int
	//implementation
	def fact(n: Int) = {
		val fac = recur(n, factCombiner(n, dec(n)))
		fac(n)
	}                                         //> fact: (n: Int)Int
	
	fact(1)                                   //> res5: Int = 1
	fact(5)                                   //> res6: Int = 120
	fact(6)                                   //> res7: Int = 720
	fact(7)                                   //> res8: Int = 5040
	
	//Problem 5++++++++++++++++++++++++++++++++++++++++++++++++++
	def parseDigits(digits: String): Option[Int] =
   if (digits.matches("[0-9]*")) Some(digits.toInt) else None
                                                  //> parseDigits: (digits: String)Option[Int]
   
   def deOptionized[A,B] (f: A=>Option[B]) = {
   	def helper(x:A) : B = {
   		if(f(x) == None)
   			throw new Exception("Wrong input")
   		else
   			f(x).get
   	}
   	helper _
   }                                              //> deOptionized: [A, B](f: A => Option[B])A => B
   
   val g2 = deOptionized(parseDigits _)           //> g2  : String => Int = FunctionLab$$$Lambda$23/3447021@1a407d53
   try {
   	println(g2("12345"))
   	println(g2("56"))
   	println(g2("abc"))
   	println(g2("5")) // not printed because already caught String: abc
   }
   catch {
   	case e: Exception => println(e)
   }                                              //> 12345
                                                  //| 56
                                                  //| java.lang.Exception: Wrong input
   
}