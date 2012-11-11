package week1_FunctionsEvaluations

object Example1_NewtonSquareRoot {

  def abs(x: Double) =
    if (x > 0) x else -x                          //> abs: (x: Double)Double

  def squareRoot(x: Double) =
    squareRootIter(1, x)                          //> squareRoot: (x: Double)Double

  def squareRootIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else squareRootIter(getNewGuess(guess, x), x) //> squareRootIter: (guess: Double, x: Double)Double
    
  def getNewGuess(guess: Double, x: Double): Double =
    (guess + x / guess) / 2                       //> getNewGuess: (guess: Double, x: Double)Double
    
  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs((guess * guess) - x) < x * 0.05           //> isGoodEnough: (guess: Double, x: Double)Boolean
  
  // Testing it works
  squareRoot(10)                                  //> res0: Double = 3.196005081874647
  
}