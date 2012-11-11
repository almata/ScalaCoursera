package week5_Lists

object Example2_RemovingAnElement {

  def removeAt1[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => xs
    case y :: ys => if (n == 0) ys else y :: removeAt1(ys, n - 1)
  }                                               //> removeAt1: [T](xs: List[T], n: Int)List[T]
  
  def removeAt2[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n + 1)
                                                  //> removeAt2: [T](xs: List[T], n: Int)List[T]
  def removeAt3[T](xs: List[T], n: Int) = (xs take n) ++ (xs drop n + 1)
                                                  //> removeAt3: [T](xs: List[T], n: Int)List[T]
  
  // Testing it works
  removeAt1(List('a', 'b', 'c', 'd'), 2)          //> res0: List[Char] = List(a, b, d)
  removeAt2(List('a', 'b', 'c', 'd'), 2)          //> res1: List[Char] = List(a, b, d)
  removeAt3(List('a', 'b', 'c', 'd'), 2)          //> res2: List[Char] = List(a, b, d)
  
}