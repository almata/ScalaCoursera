package week6_Collections

object Example5_Maps {

  val romanLetters = Map("I" -> 1, "V" -> 5, "X" -> 10)
                                                  //> romanLetters  : scala.collection.immutable.Map[java.lang.String,Int] = Map(I
                                                  //|  -> 1, V -> 5, X -> 10)

  val romanNumbers = romanLetters map {
    case (x, y) => (y, x)
  }                                               //> romanNumbers  : scala.collection.immutable.Map[Int,java.lang.String] = Map(1
                                                  //|  -> I, 5 -> V, 10 -> X)
 
  // Testing it works
  romanNumbers(5)                                 //> res0: java.lang.String = V
  romanNumbers get 5                              //> res1: Option[java.lang.String] = Some(V)
  romanNumbers get 6                              //> res2: Option[java.lang.String] = None
  val rn = romanNumbers withDefaultValue "-"      //> rn  : scala.collection.immutable.Map[Int,java.lang.String] = Map(1 -> I, 5 -
                                                  //| > V, 10 -> X)
  rn(5)                                           //> res3: java.lang.String = V
  rn(6)                                           //> res4: java.lang.String = -

  def showRoman(number: Int) = romanNumbers get(number) match {
    case Some(letter) => letter
    case None => "no Roman letter for this number"
  }                                               //> showRoman: (number: Int)java.lang.String

  // Testing it works
  showRoman(10)                                   //> res5: java.lang.String = X
  showRoman(13)                                   //> res6: java.lang.String = no Roman letter for this number

  // Order by & group by operations
  val fruit = List("apple", "pear", "orange", "pineapple")
                                                  //> fruit  : List[java.lang.String] = List(apple, pear, orange, pineapple)
  fruit sortWith(_.length < _.length)             //> res7: List[java.lang.String] = List(pear, apple, orange, pineapple)
  fruit.sorted                                    //> res8: List[java.lang.String] = List(apple, orange, pear, pineapple)
  fruit groupBy(_.head)                           //> res9: scala.collection.immutable.Map[Char,List[java.lang.String]] = Map(a ->
                                                  //|  List(apple), o -> List(orange), p -> List(pear, pineapple))

}