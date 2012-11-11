package week7_LazyEvaluation

object Example2_WaterPouringProblem {
  
  // Testing it works
  val problem = new Pouring(Vector(4, 7))         //> problem  : week7_LazyEvaluation.Pouring = week7_LazyEvaluation.Pouring@3b835
                                                  //| 282
  problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with w
                                                  //| eek7_LazyEvaluation.Example2_WaterPouringProblem.problem.Move] = Vector(Empt
                                                  //| y(0), Empty(1), Fill(0), Fill(1), Pour(0,1), Pour(1,0))
  problem.solutions(5).toList                     //> res1: List[week7_LazyEvaluation.Example2_WaterPouringProblem.problem.Path] =
                                                  //|  List(Fill(0) Pour(0,1) Fill(0) Pour(0,1) Empty(1) Pour(0,1) Fill(0) Pour(0,
                                                  //| 1) ==> Vector(0, 5)
                                                  //| , Fill(0) Pour(0,1) Fill(0) Pour(0,1) Empty(1) Pour(0,1) Fill(0) Pour(0,1) F
                                                  //| ill(0) ==> Vector(4, 5)
                                                  //| )
}