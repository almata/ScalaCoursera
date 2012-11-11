package week2_HigherOrderFunctions

object Example3_Currying {

  def triple(x: Int) = x * 3                      //> triple: (x: Int)Int

  def newCalculate(f: Int => Int): Int => Int = {
    def newCalculateF(a: Int): Int =
      if (a == 0) 0
      else f(a) + newCalculateF(a - 1)
    newCalculateF
  }                                               //> newCalculate: (f: Int => Int)Int => Int

  // Testing it works
  newCalculate(triple) (3)                        //> res0: Int = 18
  newCalculate(x => x * 3) (3)                    //> res1: Int = 18

  def newCalculateShorter(f: Int => Int)(a: Int): Int =
    if (a == 0) 0
    else f(a) + newCalculateShorter(f)(a - 1)     //> newCalculateShorter: (f: Int => Int)(a: Int)Int

  // Testing it works
  newCalculateShorter(triple)(3)                  //> res2: Int = 18

}