package week6_Collections

object Example6_Polynomials {

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    
    val terms = terms0 withDefaultValue 0.0
    
    def + (other: Poly) = new Poly(this.terms ++ (other.terms map adjust))
    
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }
    
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  // Testing it works
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //> p1  : week6_Collections.Example6_Polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
                                                  //| 
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)           //> p2  : week6_Collections.Example6_Polynomials.Poly = 7.0x^3 + 3.0x^0
  p1 + p2                                         //> res0: week6_Collections.Example6_Polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^
                                                  //| 1 + 3.0x^0
}