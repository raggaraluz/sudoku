package sudoku.algorithms

import sudoku._

trait Algorithm {
  def nextMove(board: Board) : List[Move]

  def cleanBoard(board: Board) : Board = board
}
