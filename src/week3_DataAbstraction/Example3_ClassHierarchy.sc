package week3_DataAbstraction

object Example3_ClassHierarchy {

  // Testing it works
  val t1 = new NonEmpty(3, new Empty, new Empty)  //> t1  : week3_DataAbstraction.NonEmpty = {.3.}
  val t2 = t1.incl(4)                             //> t2  : week3_DataAbstraction.IntSet = {.3{.4.}}

}

abstract class IntSet {

  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  
}

class Empty extends IntSet {

  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

}