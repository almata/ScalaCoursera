package week7_LazyEvaluation

object Example1_Streams {

  def streamRange1(lo: Int, hi: Int): Stream[Int] =
    (lo until hi).toStream                        //> streamRange1: (lo: Int, hi: Int)Stream[Int]

  def streamRange2(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange2(lo + 1, hi))//> streamRange2: (lo: Int, hi: Int)Stream[Int]

  // Testing it works
  streamRange1(5,15)                              //> res0: Stream[Int] = Stream(5, ?)
  streamRange2(5,15)                              //> res1: Stream[Int] = Stream(5, ?)

  def from(n: Int): Stream[Int] = n #:: from(n+1) //> from: (n: Int)Stream[Int]

  // Testing it works
  val nats = from(0)                              //> nats  : Stream[Int] = Stream(0, ?)
  val m4s = nats map (_ * 4)                      //> m4s  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
  (m4s take 10).toList                            //> res2: List[Int] = List(0, 4, 8, 12, 16, 20, 24, 28, 32, 36)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
                                                  //> sieve: (s: Stream[Int])Stream[Int]
  
  // Testing it works
  (sieve(from(2)) take 10).toList                 //> res3: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }                                               //> sqrtStream: (x: Double)Stream[Double]

  // Testing it works
  sqrtStream(4).take(10).toList                   //> res4: List[Double] = List(1.0, 2.5, 2.05, 2.000609756097561, 2.0000000929222
                                                  //| 947, 2.000000000000002, 2.0, 2.0, 2.0, 2.0)

}