package week4_TypesPatternMatching

trait Expr1 {

  def isNumber: Boolean  // classification
  def isSum: Boolean     // classification
  def numValue: Int      // accessor
  def leftOp: Expr1      // accessor
  def rightOp: Expr1     // accessor

}

class Number1(n: Int) extends Expr1 {

  def isNumber: Boolean = true
  def isSum: Boolean = false
  def numValue: Int = n
  def leftOp: Expr1 = throw new Error("Number.leftOp")
  def rightOp: Expr1 = throw new Error("Number.rightOp")
  
}

class Sum1(e1: Expr1, e2: Expr1) extends Expr1 {
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr1 = e1
  def rightOp: Expr1 = e2
  
}

object test1 {
  
  def eval1(e: Expr1): Int =
    if (e.isNumber) e.numValue
    else if (e.isSum) eval1(e.leftOp) + eval1(e.rightOp)
    else throw new Error("Unknown expression " + e)

  // Non-recommended solution
  def eval2(e: Expr1): Int =
  	if (e.isInstanceOf[Number1]) e.asInstanceOf[Number1].numValue
  	else if (e.isInstanceOf[Sum1]) eval2(e.asInstanceOf[Sum1].leftOp) + eval2(e.asInstanceOf[Sum1].rightOp)
  	else throw new Error("Unknown expression " + e)
  
}