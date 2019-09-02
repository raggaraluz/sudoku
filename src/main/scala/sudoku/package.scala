package object sudoku {
  /**
    * Current Sudoku state
    */
  type State = Vector[Square]

  /**
    * Position in the board
    */
  type Position = (Int, Int)

  /**
    * Group of positions
    */
  type Group = Seq[Int]
}
