package week3_DataAbstraction

object Example1_Classes {

  // Testing it works
  val x = new Rational(1, 2)                      //> x  : week3_DataAbstraction.Rational = 1/2
  val y = new Rational(3, 4)                      //> y  : week3_DataAbstraction.Rational = 3/4
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2
  x.add(y)                                        //> res2: week3_DataAbstraction.Rational = 5/4
  y.sub(x)                                        //> res3: week3_DataAbstraction.Rational = 1/4
  x.less(y)                                       //> res4: Boolean = true
  x.max(y)                                        //> res5: week3_DataAbstraction.Rational = 3/4
  val z = new Rational(5)                         //> z  : week3_DataAbstraction.Rational = 5/1
  
}

class Rational(x: Int, y:Int) {
  require(y != 0, "denominator cannot be zero")

  // Useful to show 1/4 instead of 2/8
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer = x / g // numerator
  def denom = y / g // denominator
  
  // Second constructor (using just a numerator)
  def this(x: Int) = this(x, 1)
  
  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)
      
  def neg: Rational =
    new Rational(-this.numer, this.denom)

  def sub(that: Rational) =
    this.add(that.neg)
      
  def less(that: Rational) =
    this.numer * that.denom < that.numer * this.denom
    
  def max(that: Rational) =
    if (this.less(that)) that else this
      
  // Useful to customize how an object has to be shown
  override def toString =
    this.numer + "/" + this.denom
    
}