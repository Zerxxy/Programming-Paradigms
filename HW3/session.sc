/*
*	@author David Vu
*/
object session {
 
// ++++++++++++++++++
// PROBLEM 1
def compose[A, B, C](f: B => C, g: A => B): A => C = {
	def helper(x: A): C = f(g(x))
	helper _
}                                                 //> compose: [A, B, C](f: B => C, g: A => B)A => C

// Test functions
def f(b: Int): Int = b / 2                        //> f: (b: Int)Int
def g(a: Int): Int = a + 2                        //> g: (a: Int)Int

// ** OUTPUT **
val f_g = compose(f,g)                            //> f_g  : Int => Int = session$$$Lambda$10/431687835@5ba23b66
val g_f = compose(g,f)                            //> g_f  : Int => Int = session$$$Lambda$10/431687835@3f0ee7cb
f_g(0)                                            //> res0: Int = 1
g_f(0)                                            //> res1: Int = 2


// ++++++++++++++++++
// PROBLEM 2
def dec(n: Int) = {n-1}                           //> dec: (n: Int)Int
def isZero(n: Int) = {n == 0}                     //> isZero: (n: Int)Boolean

def selfIter[T](f: T=>T,n: Int): T => T = {
	val initF = f
	def helper(f: T=>T, n: Int): T => T = {
			if(isZero(n)){f}
			else{helper(compose(f,initF), dec(n))}
	}
	helper(f,n)
}                                                 //> selfIter: [T](f: T => T, n: Int)T => T

// Test functions
def inc(n: Double) = n+1                          //> inc: (n: Double)Double
def double(x: Double) = 2 * x                     //> double: (x: Double)Double

// ** OUTPUT **
val inc_composed_23 = selfIter(inc _, 23)(4)      //> inc_composed_23  : Double = 28.0
val double_composed_13 = selfIter(double _, 13)(3)//> double_composed_13  : Double = 49152.0


// ++++++++++++++++++
// PROBLEM 3

def test[T](element: T): Boolean = {
	element match {
		case element: String => true
		case _ => false
	}
}                                                 //> test: [T](element: T)Boolean

def countPass[T](arr: Array[T]): Int = {
	var count = 0
	for(element <- arr) {
		if(test(element)){count = count + 1}
	}
	count
}                                                 //> countPass: [T](arr: Array[T])Int
// ** OUTPUT **
var list_1 = Array(0,59,13,-52,34)                //> list_1  : Array[Int] = Array(0, 59, 13, -52, 34)
var list_2 = Array("dog",59,"foo",-52,34.34)      //> list_2  : Array[Any] = Array(dog, 59, foo, -52, 34.34)

countPass(list_1)                                 //> res2: Int = 0
countPass(list_2)                                 //> res3: Int = 2

// ++++++++++++++++++
// PROBLEM 4


// Classic recursion
def fact(n: Int): Int = {
	if(isZero(n)){1}
	else{n * fact(dec(n))}
}                                                 //> fact: (n: Int)Int

// 4a implementation
def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
	def helper(baseVal: Int): Int = {
		if(isZero(baseVal)){1}
		else{combiner(baseVal, helper(dec(baseVal)))}
	}
	helper _
}                                                 //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int

// 4b implementation
def factorialCombiner(a: Int, b: Int): (Int,Int)=>Int = {
	def helper(arg1: Int, arg2: Int): Int = {
		arg1 * arg2
	}
	helper _
}                                                 //> factorialCombiner: (a: Int, b: Int)(Int, Int) => Int

def fac(n: Int): Int = {
	val f = recur(n, factorialCombiner(n, dec(n)))
	f(n)
}                                                 //> fac: (n: Int)Int

// ** OUTPUT **
fac(0)                                            //> res4: Int = 1
fac(1)                                            //> res5: Int = 1
fac(2)                                            //> res6: Int = 2
fac(3)                                            //> res7: Int = 6
fac(5)                                            //> res8: Int = 120

// ++++++++++++++++++
// PROBLEM 5

// Tester function
def parseDigits(digits: String): Option[Int] = {
   if (digits.matches("[0-9]*")) Some(digits.toInt) else None
}                                                 //> parseDigits: (digits: String)Option[Int]

// Main function
def deOptionize[A,B](f: A=>Option[B]): A=>B = {
	def helper(x: A): B = {
		if(f(x).isEmpty){throw new Exception("Null")}
		else{f(x).get}
	}
	helper _
}                                                 //> deOptionize: [A, B](f: A => Option[B])A => B

// ** OUTPUT **
val g_deOptionize = deOptionize(parseDigits _)    //> g_deOptionize  : String => Int = session$$$Lambda$24/1068934215@79b4d0f
try{
println(g_deOptionize("21390"))
println(g_deOptionize("da89"))
}catch{case e: Exception => println(e)}           //> 21390
                                                  //| java.lang.Exception: Null

}