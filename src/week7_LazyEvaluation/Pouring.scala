package week7_LazyEvaluation

class Pouring(capacity: Vector[Int]) {

  // State
  
  type State = Vector[Int]
  val initialState = capacity map (x => 0)
  
  // Moves
  
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(fromGlass: Int, toGlass: Int) extends Move {
    def change(state: State) = {
      val amount = state(fromGlass) min (capacity(toGlass) - state(toGlass))
      state updated (fromGlass, state(fromGlass) - amount) updated (toGlass, state(toGlass) + amount)
    }
  }
  
  val glasses = 0 until capacity.length
  
  val moves = 
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))
  
  // Paths
    
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + " ==> " + endState + "\n"
  }  
  
  val initialPath = new Path(Nil, initialState)
    
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
  	if (paths.isEmpty) Stream.empty
  	else {
  	  val more = for {
  		  path <- paths
  		  next <- moves map path.extend
  		  if !(explored contains next.endState)
  	    } yield next
  	  paths #:: from(more, explored ++ (more map (_.endState)))
  }
  
  val pathSets = from(Set(initialPath), Set(initialState))
  
  def solutions(target: Int): Stream[Path] =
  	for {
  	  pathSet <- pathSets
  	  path <- pathSet
  	  if path.endState contains target
  	} yield path
  
}