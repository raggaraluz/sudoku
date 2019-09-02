package sudoku

class Board private (val state : State, val layout: Layout) {
  private def this(layout: Layout) = {
    this(layout.emptyState, layout)
  }

  def move(move: Move) : Board = {
    if (!checkValid(move)) {
      throw new Exception("Invalid move: " + move + "\n" + toString)
    }
    val newState = updateState(state, move)
    new Board(newState, layout)
  }

  def invalidate(move: Move) : Board = {
    val newState = removeAvailableSq(move.value)(state, layout.posToIdx(move.pos))
    new Board(newState, layout)
  }

  def isResolved : Boolean = {
    state.filter(_.isEmpty).isEmpty
  }

  private def removeAvailable(value: Int, state: State, indexes: Traversable[Int]): State = {
    indexes.foldLeft(state)(removeAvailableSq(value))
  }

  private def removeAvailableSq(value: Int)(state: State, idx : Int) : State = {
    val toUpdate = state(idx)

    state.updated(idx, toUpdate.copy(free = toUpdate.free - value))
  }

  private def checkValid(move: Move) : Boolean = {
    // Checks if the move is available
    state(layout.posToIdx(move.pos)).free.contains(move.value)
  }

  private def updateState(state: State, move: Move): State = {
    val idx = layout.groups(move).flatten.distinct

    // Remove all items
    removeAvailable(move.value, state, idx).
      // And set the square of the move
      updated(layout.posToIdx(move.pos), Square(move))
  }

  override def toString : String= {
    "Values: \n" + layout.printBoard(state)
  }

}


object Board {
  def apply(layout: Layout): Board = {
    new Board(layout)
  }

  def apply(layout: Layout, file: String): Board = {
    val board = new Board(layout)
    val moves = layout.parse(file)
    moves.foldLeft(board)(_.move(_))
  }
}
