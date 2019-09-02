package sudoku.layouts
import sudoku._
import scala.io.Source

object Sucoku extends Layout {
  protected val SIZE = 12
  protected val SIZE_SQR_ROW = 3
  protected val SIZE_SQR_COLUMN = 4

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


  def printBoard(vectorStates: State) : String= {
    val vector = vectorStates.map(x => f"${x.value.getOrElse(0)}%2d")
    val numbers = for {
      row <- 0 until SIZE
      offset = row * SIZE
      first = vector.slice(offset, offset + SIZE_SQR_COLUMN).mkString(" ")
      second = vector.slice(offset + SIZE_SQR_COLUMN, offset + 2 * SIZE_SQR_COLUMN).mkString(" ")
      third = vector.slice(offset + 2 * SIZE_SQR_COLUMN, offset + SIZE).mkString(" ")
    } yield s"| ${first} | ${second} | ${third} |"

    val separator = List("+-------------+-------------+-------------+")
 
    val lines = separator ++ numbers.slice(0, SIZE_SQR_ROW) ++
      separator ++ numbers.slice(SIZE_SQR_ROW, 2 * SIZE_SQR_ROW) ++
      separator ++ numbers.slice(2 * SIZE_SQR_ROW, 3 * SIZE_SQR_ROW) ++
      separator ++ numbers.slice(3 * SIZE_SQR_ROW, SIZE) ++ separator
    lines.mkString("\n").replace(" 0", "  ")
  }

  def parse(file: String): List[Move] = {
    val moves = for {
      (line, row) <- Source.fromFile(file).getLines.zipWithIndex
      (str, column) <- line.sliding(2, 2).zipWithIndex
      if str.trim != ""
      num = str.trim.toInt
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
      corner_row <- (0 until SIZE_SQR_COLUMN).map(_ * SIZE_SQR_ROW)
      corner_column <- (0 until SIZE_SQR_ROW).map(_ * SIZE_SQR_COLUMN)
    } yield (for {
      row <- corner_row until corner_row + SIZE_SQR_ROW
      column <- corner_column until (corner_column + SIZE_SQR_COLUMN)
      idx = posToIdx(row, column)
    } yield idx)

    rows ++ columns ++ squares

  }

}
