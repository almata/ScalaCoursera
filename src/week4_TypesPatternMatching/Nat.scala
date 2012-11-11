package week4_TypesPatternMatching

abstract class Nat {
  
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat

}

// Sub-object for number zero
object Zero extends Nat {

  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("0 - number")
  
}

// Sub-class for strictly positive numbers
class Succ(n: Nat) extends Nat {

  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
  
}