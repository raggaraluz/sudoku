package sudoku.algorithms

import sudoku._

object FullRecursive extends Algorithm {
  def nextMove(board: Board) : List[Move] = {
    val next = board.state.zipWithIndex.filter(_._1.isEmpty).map(x => (x._1.free.size, x._2))

    if (next.isEmpty) {
      List()
    }
    else {
      val (_, idx)  = next.head
      val square = board.state(idx)

      getMoves(square)
    }
  }

  def getMoves(square: Square): List[Move] = (
    for {
      value <- square.free
    } yield Move(square.position, value)
  ).toList
}
