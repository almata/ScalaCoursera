package week2_HigherOrderFunctions

import scala.annotation.tailrec

object Example1_TailRecursion {

  // Testing it works
  new TailRecursion().gcd(14,21)                  //> res0: Int = 7
  
  // Testing it works
  new TailRecursion().factorial(10)               //> res1: Int = 3628800

  // Testing it works
  new TailRecursion().factorialTR(10)             //> res2: Int = 3628800
  
}

class TailRecursion {

  @tailrec
  final def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  // Non tail recursive factorial
  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)

  // Tail recursive factorial
  def factorialTR(n: Int): Int = {
    @tailrec
    def loop(n: Int, acum: Int): Int =
      if (n == 0) acum else loop(n - 1, acum * n)
    loop(n, 1)
  }

}