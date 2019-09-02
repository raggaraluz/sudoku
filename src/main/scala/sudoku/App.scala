package sudoku

import layouts._
import algorithms._

object Sudoku extends App {
//  val board = Board(Regular, "src/main/resources/sudoku51.txt")
//  val board = Board(Sucoku, "src/main/resources/sucoku1.txt")

  val board = Board(Samurai, "src/main/resources/samurai85.txt")

  println(board)

  val solved = Solver.solve(board, Human2)

  println(solved)

  println(Samurai.groups(Move((6, 6), 1)))
}

