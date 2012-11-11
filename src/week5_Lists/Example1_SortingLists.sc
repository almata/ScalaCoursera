package week5_Lists

object Example1_SortingLists {

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]

  // Testing it works
  val myList = 4 :: 2 :: 5 :: 1 :: 3 :: Nil       //> myList  : List[Int] = List(4, 2, 5, 1, 3)
  isort(myList)                                   //> res0: List[Int] = List(1, 2, 3, 4, 5)
  
}