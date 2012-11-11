package week4_TypesPatternMatching

trait Expr3 {
  
  // First option: defining eval as an internal method
  def eval: Int = this match {
    case Number3(n) => n
    case Sum3(e1, e2) => e1.eval + e2.eval
  }
  
}

case class Number3(n: Int) extends Expr3

case class Sum3(e1: Expr3, e2: Expr3) extends Expr3

object test3 {
  
  // Second option: defining eval as an external function
  def eval(e: Expr3): Int = e match {
  	case Number3(n) => n
  	case Sum3(e1, e2) => eval(e1) + eval(e2)
  }
  
  
  def show(e: Expr3): String = e match {
    case Number3(n) => n.toString
    case Sum3(e1, e2) => show(e1) + " + " + show(e2)
  }
  
}