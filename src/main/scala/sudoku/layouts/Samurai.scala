package sudoku.layouts
import sudoku._
import scala.io.Source

object Samurai extends Layout {
  private val SIZE = 9 * 2 + 3

  private def isInvalid(position: Position) : Boolean = {
    val (x, y) = position
    ((x < 6 || x > 14) && y > 8 && y < 12) ||
    ((y < 6 || y > 14) && x > 8 && x < 12)
  }

  private lazy val posIdx = {
    for {
      x <- 0 until SIZE
      y <- 0 until SIZE
      if !isInvalid((x, y))
    } yield (x, y)
  }

  private lazy val posIdxMap = posIdx.zipWithIndex.toMap
  def posToIdx(position : Position) : Int = posIdxMap(position)

  private lazy val idxPosMap = posIdxMap.map{ case (pos, idx) =>(idx, pos) }
  def idxToPos(idx: Int) : Position = idxPosMap(idx)

  def emptyState : State = {
    (for {
      (x, y) <- posIdx
      square = Square((x, y), maxValue)
    } yield square).toVector
  }

  val maxValue = 9

  def printBoard(vectorStates: State) : String= {
    val numbers = for {
      x <- 0 until SIZE
      y <- 0 until SIZE
      invalid = isInvalid(x, y)
    } yield
        if (invalid) {
          " "
        } else {
          vectorStates(posToIdx(x, y)).toString
        }

    val separator = " +-------+-------+-------+-------+-------+-------+-------+\n"
    numbers.sliding(3, 3).map(" | " + _.mkString(" "))
      .sliding(7, 7).map(_.mkString + " |\n")
      .sliding(3, 3).map(separator + _.mkString)
      .mkString + separator
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
    val offsets = List((0, 0), (0, 12), (12, 0), (12, 12), (6, 6))

    val rows = for {
      (off_x, off_y) <- offsets
      x <- off_x until off_x + 9
    } yield (for {
      y <- off_y until off_y + 9
      idx = posToIdx(x, y)
    } yield(idx))

    val columns = rows.map(_.map(
      idx => {
        val (x, y) = idxToPos(idx)
        posToIdx((y, x))
      }
    ))

    val squares = for {
      (off_x, off_y) <- offsets
      corner_x <- off_x until off_x + 9 by 3
      corner_y <- off_y until off_y + 9 by 3
    } yield (for {
      x <- corner_x until corner_x + 3
      y <- corner_y until corner_y + 3
      idx = posToIdx(x, y)
    } yield idx)

    (rows ++ columns ++ squares).distinct
  }

}
