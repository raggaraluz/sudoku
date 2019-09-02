package sudoku.algorithms

import sudoku._

object Human2 extends Algorithm {
  def nextMove(board: Board) : List[Move] = Human1.nextMove(board)

  override def cleanBoard(board: Board): Board = {
    cleanPairs(board)
  }

  private def cleanPairs(board: Board): Board = {
    board.layout.groups.foldLeft(board)(cleanPairs)
  }

  private def cleanPairs(board: Board, group: Group) : Board = {
    val pairs = for {
      idx <- group
      free = board.state(idx).free if (free.size == 2)
    } yield free

    val elements = for {
      elements <- pairs.groupBy(identity).values if elements.size == 2
      value <- elements(0)
    } yield value

    val idxToRemove = for {
      idx <- group if board.state(idx).free.size > 2
    } yield idx

    idxToRemove.foldLeft(board)(invalidate(elements))
  }

  private def invalidate(elements: Iterable[Int])(board: Board, idx: Int): Board = {
    def invalidate(board: Board, value: Int) : Board = {
      val move = Move(board.layout.idxToPos(idx), value)
      board.invalidate(move)
    }

    elements.foldLeft(board)(invalidate)
  }
}
