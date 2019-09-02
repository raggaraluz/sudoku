package sudoku.layouts
import sudoku._
import scala.io.Source

object Regular extends Layout {
  protected val SIZE = 9
  protected val SIZE_SQR = 3

  def posToIdx(position : Position) : Int = {
    val (x, y) = position
    x * SIZE + y
  }

  def idxToPos(idx: Int) : Position = {
    (idx / SIZE, idx % SIZE)
  }

  def emptyState : State = {
    (for {
      x <- 0 until SIZE
      y <- 0 until SIZE
      square = Square((x, y), SIZE)
    } yield square).toVector
  }

  val maxValue = SIZE

  def printBoard(vector: State) : String= {
    val numbers = for {
      row <- 0 until SIZE
      offset = row * SIZE
      first = vector.slice(offset, offset + SIZE / 3).mkString(" ")
      second = vector.slice(offset + SIZE / 3, offset + 2 * SIZE / 3).mkString(" ")
      third = vector.slice(offset + 2 * SIZE / 3, offset + SIZE).mkString(" ")
    } yield s"| ${first} | ${second} | ${third} |"

    val separator = List("+-------+-------+-------+")
 
    val lines = separator ++ numbers.slice(0, SIZE / 3) ++
      separator ++ numbers.slice(SIZE / 3, 2 * SIZE / 3) ++
      separator ++ numbers.slice(2 * SIZE / 3, SIZE) ++ separator
    lines.mkString("\n").replace('0', ' ')
  }

  def parse(file: String): List[Move] = {
    val moves = for {
      (line, row) <- Source.fromFile(file).getLines.zipWithIndex
      (char, column) <- line.zipWithIndex
      if char != ' '
      num = char.toString.toInt
    } yield Move((row, column), num)
    moves.toList
  }

  val groups = {
    val rows = for {
      row <- 0 until SIZE
    } yield (for {
      column <- 0 until SIZE
      idx = posToIdx((row, column))
    } yield idx)

    val columns = rows.map(_.map(
      idx => {
        val (x, y) = idxToPos(idx)
        posToIdx((y, x))
      }
    ))

    val squares = for {
      corner_row <- (0 until SIZE_SQR).map(_ * SIZE_SQR)
      corner_column <- (0 until SIZE_SQR).map(_ * SIZE_SQR)
    } yield (for {
      row <- corner_row until corner_row + SIZE_SQR
      column <- corner_column until (corner_column + SIZE_SQR)
      idx = posToIdx(row, column)
    } yield idx)

    rows ++ columns ++ squares

  }

}
