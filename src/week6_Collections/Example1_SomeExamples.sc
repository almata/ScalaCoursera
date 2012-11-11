package week6_Collections

object Example1_SomeExamples {

  // Example 1.1
  val M = 3                                       //> M  : Int = 3
  val N = 5                                       //> N  : Int = 5

  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,3), (1,4), (1,5), (2,1), (2,2), (2,3), (2,4), (2,5), (3,1), (3,2), (3,
                                                  //| 3), (3,4), (3,5))

  // Example 1.2
  def scalarProduct1(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum      //> scalarProduct1: (xs: Vector[Double], ys: Vector[Double])Double

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{ case (x, y) => x * y }.sum   //> scalarProduct2: (xs: Vector[Double], ys: Vector[Double])Double

  // Example 1.3
  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean

  isPrime(7)                                      //> res1: Boolean = true
  isPrime(14)                                     //> res2: Boolean = false

}