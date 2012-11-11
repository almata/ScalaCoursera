package week4_TypesPatternMatching

trait Expr2 {
  def eval: Int
}

class Number2(n: Int) extends Expr2 {
  def eval: Int = n
}

class Sum2(e1: Expr2, e2: Expr2) extends Expr2 {
  def eval: Int = e1.eval + e2.eval
}

