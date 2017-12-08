object RecurssionSession {
		def inc(n: Int) = n + 1           //> inc: (n: Int)Int
		def dec(n: Int) = n - 1           //> dec: (n: Int)Int
		def isZero(n: Int) = n == 0       //> isZero: (n: Int)Boolean
		
		// Problem 1++++++++++++++++++++++++++++++++++++++++++++++++++++
		def add(n: Int, m: Int) : Int = {
			if (isZero(m))
				n
			else
				add(inc(n),dec(m))
		}                                 //> add: (n: Int, m: Int)Int
		
		add(3,4)                          //> res0: Int = 7
		add(5,5)                          //> res1: Int = 10
		
		// Problem 2++++++++++++++++++++++++++++++++++++++++++++++++++++
		def mul(n: Int, m: Int) = {
			def helper(count: Int, result: Int) : Int = {
				if (isZero(count))
					result
				else
					helper(dec(count), add(result,m))
			}
			helper(n,0)
		}                                 //> mul: (n: Int, m: Int)Int
		
		mul(3,5)                          //> res2: Int = 15
		mul(5,5)                          //> res3: Int = 25
		mul(10,10)                        //> res4: Int = 100
		
		// Problem 3++++++++++++++++++++++++++++++++++++++++++++++++++++++
		def exp2(m: Int) = {
			def helper(count: Int, result: Int) : Int = {
				if ( isZero(count) )
					result
				else
					helper(dec(count), mul(result,2))
			}
			helper(dec(m), 2)
		}                                 //> exp2: (m: Int)Int
		
		exp2(5)                           //> res5: Int = 32
		exp2(2)                           //> res6: Int = 4
		exp2(4)                           //> res7: Int = 16
		
		// Problem 4+++++++++++++++++++++++++++++++++++++++++++++++++++++++
		def hyperExp(n: Int) = {
			def helper(count:Int, exp: Int) : Int = {
				if (count == n)
					mul(exp, exp)
				else if (isZero(count))
					helper(inc(count), 1)
				else if (count == 1)
					helper(inc(count), 2)
				else
					helper(inc(count),mul(exp,exp))
			}
			helper(0,0)
		}                                 //> hyperExp: (n: Int)Int
		
		hyperExp(4)                       //> res8: Int = 256
		hyperExp(3)                       //> res9: Int = 16
		hyperExp(5)                       //> res10: Int = 65536
		
		//Problem 5 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		//2-4 Are already tail recursion, so I will only redo #1
		def add2(n: Int, m: Int) = {
			def helper(count: Int, result: Int) : Int = {
				if (isZero(count))
					result
				else
					helper(dec(count), inc(result))
			}
			helper(m,n)
		}                                 //> add2: (n: Int, m: Int)Int
		
		add2(2,3)                         //> res11: Int = 5
		add2(5,5)                         //> res12: Int = 10
		add2(6,7)                         //> res13: Int = 13
		
		//Problem 9+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		//Recursive
		def fib1(num: Int) : Int = {
			num match {
				case 0 | 1 => 1
				case _ => fib1(num-1) + fib1(num-2)
			}
		}                                 //> fib1: (num: Int)Int
		
		for(i <- 0 until 10) println(fib1(i))
                                                  //> 1
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 5
                                                  //| 8
                                                  //| 13
                                                  //| 21
                                                  //| 34
                                                  //| 55
                                                  
     //tail-recursion
     def fib2(num: Int) = {
     	def helper(count: Int) : Int = {
     		if (isZero(count) || count == 1) 1
     		else helper(count-1) + helper(count-2)
     	}
     	helper(num)
     }                                            //> fib2: (num: Int)Int
     
     for(i <- 0 until 10) println(fib1(i))        //> 1
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 5
                                                  //| 8
                                                  //| 13
                                                  //| 21
                                                  //| 34
                                                  //| 55
                                                  
     //Problem 10++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     def choose(n: Int, k: Int) : Int = {
     	if (isZero(k) || k==n) 1
     	else choose(n-1,k-1) + choose(n-1,k)
     }                                            //> choose: (n: Int, k: Int)Int
     
     choose(10,2)                                 //> res14: Int = 45
     choose(3,2)                                  //> res15: Int = 3
}