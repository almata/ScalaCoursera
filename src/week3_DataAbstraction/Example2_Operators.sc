package week3_DataAbstraction

object Example2_Operators {

  // Testing it works
  val x = new RationalOperator(1, 2)              //> x  : week3_DataAbstraction.RationalOperator = 1/2
  val y = new RationalOperator(3, 4)              //> y  : week3_DataAbstraction.RationalOperator = 3/4
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2
  x + y                                           //> res2: week3_DataAbstraction.RationalOperator = 5/4
  y - x                                           //> res3: week3_DataAbstraction.RationalOperator = 1/4
  x < y                                           //> res4: Boolean = true
  x max y                                         //> res5: week3_DataAbstraction.RationalOperator = 3/4
 
}

class RationalOperator(x: Int, y:Int) {
  require(y != 0, "denominator cannot be zero")

  // Useful to show 1/4 instead of 2/8
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer = x / g // numerator
  def denom = y / g // denominator
  
  // Second constructor (using just a numerator)
  def this(x: Int) = this(x, 1)
  
  def + (that: RationalOperator) =
    new RationalOperator(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)
      
  def unary_- : RationalOperator =
    new RationalOperator(-this.numer, this.denom)

  def - (that: RationalOperator) =
    this + -that
      
  def < (that: RationalOperator) =
    this.numer * that.denom < that.numer * this.denom
    
  def max(that: RationalOperator) =
    if (this < that) that else this
      
  // Useful to customize how an object has to be shown
  override def toString =
    this.numer + "/" + this.denom
    
}