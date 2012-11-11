package week6_Collections

object Example3_NQueensProblem {

  def queens(n: Int): Set[List[Int]] = {

    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens

    placeQueens(n)

  }                                               //> queens: (n: Int)Set[List[Int]]

  def isSafe(col: Int, queens: List[Int]): Boolean = {

    val row = queens.length // (1)
    val queensWithRow = (row - 1 to 0 by -1) zip queens // (2)
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r // (3)
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  // In (1) we give a row value to the new queen.
  // In (2) we convert a list like List(0, 3, 1) into a new list of pairs
  // with rows included (row, column) List((2,0), (1,3), (0,1)).
  // In (3) we check that the new column is not the same as any other's
  // queen's column and that there is no diagonal between the new queen
  // and any other.

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)(": ").updated(col,"X ").mkString
    "\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])java.lang.String


  // Testing it works
  queens(4)                                       //> res0: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
  (queens(4) map show) mkString "\n"              //> res1: String = "
                                                  //| : : X : 
                                                  //| X : : : 
                                                  //| : : : X 
                                                  //| : X : : 
                                                  //| 
                                                  //| : X : : 
                                                  //| : : : X 
                                                  //| X : : : 
                                                  //| : : X : "

  (queens(8) take 2 map show) mkString "\n"       //> res2: String = "
                                                  //| : : : : X : : : 
                                                  //| : : X : : : : : 
                                                  //| X : : : : : : : 
                                                  //| : : : : : X : : 
                                                  //| : : : : : : : X 
                                                  //| : X : : : : : : 
                                                  //| : : : X : : : : 
                                                  //| : : : : : : X : 
                                                  //| 
                                                  //| : X : : : : : : 
                                                  //| : : : X : : : : 
                                                  //| : : : : : X : : 
                                                  //| : : : : : : : X 
                                                  //| : : X : : : : : 
                                                  //| X : : : : : : : 
                                                  //| : : : : : : X : 
                                                  //| : : : : X : : : "

}