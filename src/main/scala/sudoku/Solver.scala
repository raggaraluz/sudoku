package sudoku

import algorithms._

object Solver {
  def solve(board: Board, algorithm: Algorithm): Board = {

    def solve0(boardInput: Board, moves: List[Move], history: List[Board], chances: Int) : (Board, Int) = {
      if (boardInput.isResolved) {
        (boardInput, chances)
      }
      else {
        val board = algorithm.cleanBoard(boardInput)
        val nextMoves = algorithm.nextMove(board)
        nextMoves match {
          case List(move) => {
            solve0(board.move(move), moves, history, chances)
          }
          case next :: others => {
            solve0(board.move(next), next :: moves, board :: history, chances + 1)
          }
          case _ if moves.isEmpty || board.isResolved => {
            (board, chances)
          }
          case _ => {
            val move = moves.head
            val board = history.head.invalidate(move)
            solve0(board, moves.tail, history.tail, chances)
          }
        }
      }
    }
    val (solution, chances) = solve0(board, List[Move](), List[Board](), 0)
    println(s"Total chances: ${chances}")
    solution
  }


}
