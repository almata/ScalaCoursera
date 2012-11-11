package week2_HigherOrderFunctions

object Example2_HigherOrderFunctions {
  
  def calculate(a: Int, f: Int => Int): Int =
    if (a == 0) 0
    else f(a) + calculate(a - 1, f)               //> calculate: (a: Int, f: Int => Int)Int

	def single(x: Int) = x * 1                //> single: (x: Int)Int
	def double(x: Int) = x * 2                //> double: (x: Int)Int
	def triple(x: Int) = x * 3                //> triple: (x: Int)Int
	
  // Testing it works
	calculate(3, single)                      //> res0: Int = 6
	calculate(3, double)                      //> res1: Int = 12
	calculate(3, triple)                      //> res2: Int = 18

  // Testing it works
	calculate(3, (x: Int) => x * 1)           //> res3: Int = 6
	calculate(3, (x: Int) => x * 2)           //> res4: Int = 12
	calculate(3, (x: Int) => x * 3)           //> res5: Int = 18

  // Testing it works
	calculate(3, x => x * 1)                  //> res6: Int = 6
	calculate(3, x => x * 2)                  //> res7: Int = 12
	calculate(3, x => x * 3)                  //> res8: Int = 18

}