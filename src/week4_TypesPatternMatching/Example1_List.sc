package week4_TypesPatternMatching

import week4_TypesPatternMatching._

object Example1_List {
  
  // Creates a single element Cons-List
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week4_TypesPatternMatching.Cons[T]
              
  // Testing it works
  singleton[Int](1)                               //> res0: week4_TypesPatternMatching.Cons[Int] = week4_TypesPatternMatching.Cons
                                                  //| @4c5e176f
  singleton[Boolean](true)                        //> res1: week4_TypesPatternMatching.Cons[Boolean] = week4_TypesPatternMatching.
                                                  //| Cons@35549f94
  singleton(1)                                    //> res2: week4_TypesPatternMatching.Cons[Int] = week4_TypesPatternMatching.Cons
                                                  //| @46b8c8e6
  singleton(true)                                 //> res3: week4_TypesPatternMatching.Cons[Boolean] = week4_TypesPatternMatching.
                                                  //| Cons@d8d9850
    
  // Finds the n-th element in a Cons-List
  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException()
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)                      //> nth: [T](n: Int, xs: week4_TypesPatternMatching.List[T])T

  // Testing it works
  val x = new Cons(0, new Cons(10, new Cons(20, new Cons(30, new Nil))))
                                                  //> x  : week4_TypesPatternMatching.Cons[Int] = week4_TypesPatternMatching.Cons@
                                                  //| 4b0ab323
  nth(3, x)                                       //> res4: Int = 30
  nth(9, x)                                       //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week4_TypesPatternMatching.Example1_List$$anonfun$main$1.nth$1(week4_
                                                  //| TypesPatternMatching.Example1_List.scala:18)
                                                  //| 	at week4_TypesPatternMatching.Example1_List$$anonfun$main$1.apply$mcV$sp
                                                  //| (week4_TypesPatternMatching.Example1_List.scala:25)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week4_TypesPatternMatching.Example1_List$.main(week4_TypesPatternMatc
                                                  //| hing.Example1_List.scala:5)
                                                  //| 	at week4_TypesPatternMatching.Example1_List.main(week4_TypesPatternMatch
                                                  //| ing.Example1_List.scala)
  
}