package week5_Lists

import scala.math.Ordering

object Example4_ImplicitParameters {

  val myList = -4 :: 2 :: 5 :: 1 :: 3 :: -9 :: Nil//> myList  : List[Int] = List(-4, 2, 5, 1, 3, -9)
  val myFruits = List("apple", "pineapple", "orange", "banana")
                                                  //> myFruits  : List[java.lang.String] = List(apple, pineapple, orange, banana)

  def abs(x: Double) =
    if (x > 0) x else -x                          //> abs: (x: Double)Double

  def msort1[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
 
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys)
                                     else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort1(fst)(lt), msort1(snd)(lt))
    }
  
  }                                               //> msort1: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]
  
  // Testing it works
  msort1(myList)((x: Int, y: Int) => x < y)       //> res0: List[Int] = List(-9, -4, 1, 2, 3, 5)
  msort1(myList)((x: Int, y: Int) => x > y)       //> res1: List[Int] = List(5, 3, 2, 1, -4, -9)
  msort1(myList)((x: Int, y: Int) => abs(x) < abs(y))
                                                  //> res2: List[Int] = List(1, 2, 3, -4, 5, -9)
  msort1(myFruits)((x: String, y: String) => x.compareTo(y) < 0)
                                                  //> res3: List[java.lang.String] = List(apple, banana, orange, pineapple)
  msort1(myFruits)((x, y) => x.compareTo(y) < 0)  //> res4: List[java.lang.String] = List(apple, banana, orange, pineapple)
  
  def msort2[T](xs: List[T])(ord: Ordering[T]): List[T] = {
 
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys)
                                     else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort2(fst)(ord), msort2(snd)(ord))
    }
  
  }                                               //> msort2: [T](xs: List[T])(ord: scala.math.Ordering[T])List[T]
  
  // Testing it works
  msort2(myList)(Ordering.Int)                    //> res5: List[Int] = List(-9, -4, 1, 2, 3, 5)
  msort2(myFruits)(Ordering.String)               //> res6: List[java.lang.String] = List(apple, banana, orange, pineapple)

  def msort3[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
 
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys)
                                     else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort3(fst), msort3(snd))
    }
  
  }                                               //> msort3: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
  
  // Testing it works
  msort3(myFruits)                                //> res7: List[java.lang.String] = List(apple, banana, orange, pineapple)
  msort3(myFruits)(Ordering.String)               //> res8: List[java.lang.String] = List(apple, banana, orange, pineapple)
  
}