object ListLab2 {

	//Problem 1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	var cs152 = List(List(93.0, 89.0, 90.0),List(50.0,40.0), List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))
                                                  //> cs152  : List[List[Double]] = List(List(93.0, 89.0, 90.0), List(50.0, 40.0),
                                                  //|  List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))
	
	def avg(scores: List[Double])  = {
		var total = 0.0
		for (x <- scores) total += x
		total/scores.length
	}                                         //> avg: (scores: List[Double])Double
	
	def avgAvg(scores: List[List[Double]]) = {
		scores.map(avg _)
	}                                         //> avgAvg: (scores: List[List[Double]])List[Double]
	
	avgAvg(cs152)                             //> res0: List[Double] = List(90.66666666666667, 45.0, 73.0, 82.66666666666667)
                                                  //| 
	
	def passed(x: Double) = x>=70             //> passed: (x: Double)Boolean
	
	def passing(pred: Double=>Boolean, scores: List[List[Double]]) = {
		val average = scores.map(avg _)
		val passStudents = average.filter(passed _)
		var index = List[Int]()
		for(i <- passStudents) {
			index ::= average.indexOf(i)
		}
		index
	}                                         //> passing: (pred: Double => Boolean, scores: List[List[Double]])List[Int]
	
	passing(passed _, cs152)                  //> res1: List[Int] = List(3, 2, 0)
	
	def add(scores:List[Double]) = {
		var total = 0.0
		for (x <- scores) total += x
		total
	}                                         //> add: (scores: List[Double])Double
	
	def sumSums(scores: List[List[Double]]) = {
		scores.map(add _).reduce(_ + _)
	}                                         //> sumSums: (scores: List[List[Double]])Double
	
	sumSums(cs152)                            //> res2: Double = 829.0
	
	//Problem 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	def spellCheck(doc: List[String], dictionary: List[String]) = {
		var wrong = List[String]()
		for(i <- doc) {
			if(!dictionary.contains(i)) wrong ::= i
		}
		wrong
	}                                         //> spellCheck: (doc: List[String], dictionary: List[String])List[String]
	
	val doc = List("Hello", "Yolo", "World", "There")
                                                  //> doc  : List[String] = List(Hello, Yolo, World, There)
	val dict = List("Hello", "World", "There")//> dict  : List[String] = List(Hello, World, There)
	
	spellCheck(doc, dict)                     //> res3: List[String] = List(Yolo)
	
	//Problem 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	def spellCheck2(doc: List[String], dictionary: List[String]) = doc.filter(!dictionary.contains(_))
                                                  //> spellCheck2: (doc: List[String], dictionary: List[String])List[String]
	
	spellCheck2(doc,dict)                     //> res4: List[String] = List(Yolo)
	
	//Problem 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	def exp(x: Double, y: Double) = {
		def helper(count: Int, result: Double) : Double = {
			if(count == 0) result
			else helper(count-1, result*x)
		}
		if(y==0) 1
		else helper(y.toInt-1,x)
	}                                         //> exp: (x: Double, y: Double)Double
	
	exp(2,5)                                  //> res5: Double = 32.0
	exp(5,0)                                  //> res6: Double = 1.0
	exp(5,1)                                  //> res7: Double = 5.0
	
	
	def evalMono(mono: (Double, Double), x: Double) = {
		val co = mono._1
		val exponential = mono._2
		
		co * exp(x,exponential)
	}                                         //> evalMono: (mono: (Double, Double), x: Double)Double
	
	//3*5^2 = 75
	evalMono((3,2),5)                         //> res8: Double = 75.0
	//4*3^4 = 324
	evalMono((4,4),3)                         //> res9: Double = 324.0
	//0*10^10 = 0
	evalMono((0,10),10)                       //> res10: Double = 0.0
	//2*2^0 = 2
	evalMono((2,0),2)                         //> res11: Double = 2.0
	
	def evalPoly(poly: List[(Double,Double)], x: Double) = {
		poly.map{case (p1, p2) => evalMono((p1,p2),x)}.reduce(_ + _)
	}                                         //> evalPoly: (poly: List[(Double, Double)], x: Double)Double
	
	//3x^2 + 2x + 5, where x = 2: 21
	val list = List((3.0,2.0),(2.0,1.0),(5.0,0.0))
                                                  //> list  : List[(Double, Double)] = List((3.0,2.0), (2.0,1.0), (5.0,0.0))
	evalPoly(list, 2)                         //> res12: Double = 21.0
}