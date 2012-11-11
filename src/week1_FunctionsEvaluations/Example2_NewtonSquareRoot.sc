package week1_FunctionsEvaluations

object Example2_NewtonSquareRoot {

  def abs(x: Double) =
    if (x > 0) x else -x                          //> abs: (x: Double)Double

  def squareRoot(x: Double) = {

	  def squareRootIter(guess: Double): Double =
	    if (isGoodEnough(guess)) guess
	    else squareRootIter(getNewGuess(guess))
	    
	  def getNewGuess(guess: Double): Double =
	    (guess + x / guess) / 2
	    
	  def isGoodEnough(guess: Double): Boolean =
	    abs((guess * guess) - x) < x * 0.05
  
    squareRootIter(1)
  }                                               //> squareRoot: (x: Double)Double
  
  // Testing it works
  squareRoot(10)                                  //> res0: Double = 3.196005081874647

}