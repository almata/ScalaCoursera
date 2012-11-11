package week5_Lists

object Example3_MergeSortingLists {

  def msort1(xs: List[Int]): List[Int] = {
 
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 => ys match {
          case Nil => xs
          case y :: ys1 => if (x < y) x :: merge(xs1, ys)
                           else y :: merge(xs, ys1)
        }
      }

      val (fst, snd) = xs splitAt n
      merge(msort1(fst), msort1(snd))
    }
  
  }                                               //> msort1: (xs: List[Int])List[Int]

  def msort2(xs: List[Int]): List[Int] = {
 
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys)
                                     else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort2(fst), msort2(snd))
    }
  
  }                                               //> msort2: (xs: List[Int])List[Int]

  // Testing it works
  val myList = 4 :: 2 :: 5 :: 1 :: 3 :: Nil       //> myList  : List[Int] = List(4, 2, 5, 1, 3)
  msort1(myList)                                  //> res0: List[Int] = List(1, 2, 3, 4, 5)
  msort2(myList)                                  //> res1: List[Int] = List(1, 2, 3, 4, 5)

}