package week5_Lists

object Example5_HigherOrderFunctions {

  def scaleList1(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList1(ys, factor)
  }                                               //> scaleList1: (xs: List[Double], factor: Double)List[Double]

  def scaleList2(xs: List[Double], factor: Double) =
    xs map (x => x * factor)                      //> scaleList2: (xs: List[Double], factor: Double)List[Double]

  // Testing it works
  scaleList1(List(1, 2, 3, 4), 2)                 //> res0: List[Double] = List(2.0, 4.0, 6.0, 8.0)
  scaleList2(List(1, 2, 3, 4), 2)                 //> res1: List[Double] = List(2.0, 4.0, 6.0, 8.0)

  def posElems1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems1(ys) else posElems1(ys)
  }                                               //> posElems1: (xs: List[Int])List[Int]

  def posElems2(xs: List[Int]): List[Int] =
    xs filter (x => x > 0)                        //> posElems2: (xs: List[Int])List[Int]
  
  // Testing it works
  posElems1(List(1, -2, 3, -4, 5))                //> res2: List[Int] = List(1, 3, 5)
  posElems2(List(1, -2, 3, -4, 5))                //> res3: List[Int] = List(1, 3, 5)

  def sum1(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum1(ys)
  }                                               //> sum1: (xs: List[Int])Int

  def sum2(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
                                                  //> sum2: (xs: List[Int])Int
  def sum3(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
                                                  //> sum3: (xs: List[Int])Int
  def sum4(xs: List[Int]) = (xs foldLeft 0) (_ + _)
                                                  //> sum4: (xs: List[Int])Int
                            
  // Testing it works
  sum1(List(1, -2, 3, -4, 5))                     //> res4: Int = 3
  sum2(List(1, -2, 3, -4, 5))                     //> res5: Int = 3
  sum3(List(1, -2, 3, -4, 5))                     //> res6: Int = 3
  sum4(List(1, -2, 3, -4, 5))                     //> res7: Int = 3
  
  // Testing it works
  val myList = List(10.0, 5.0, 2.0)               //> myList  : List[Double] = List(10.0, 5.0, 2.0)
  myList reduceLeft (_ / _)                       //> res8: Double = 1.0
  myList reduceRight (_ / _)                      //> res9: Double = 4.0
  
}