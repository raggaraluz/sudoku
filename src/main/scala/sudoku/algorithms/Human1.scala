package sudoku.algorithms

import sudoku._

object Human1 extends Algorithm {
  def nextMove(board: Board) : List[Move] = {
    val human = for {
      group <- board.layout.groups.toStream
      move <- getSingleValueMove(board, group)
    } yield move

    val move = if (!human.isEmpty) {
      print("human - ")
      human.take(1).toList
    }
    else {
      print("simple - ")
      Simple.nextMove(board)
    }
    println(s"Move: $move")
    move
  }


  def getSingleValueMove(board: Board, group: Group): Option[Move] = {
    val freeValues = for {
      idx <- group.toSeq
      Square(_, frees, pos) = board.state(idx)
      free <- frees
    } yield (free, pos)

    val valueMap = freeValues.groupBy(_._1)
    val singleValues = valueMap.mapValues(_.size).filter(_._2 == 1)

    val move = for {
      (value, _) <- singleValues.headOption
      Seq((_, pos)) = valueMap(value)
    } yield Move(pos, value)

    // if (move != None) {
    //   println(s"    Group: ${group.map(board.layout.idxToPos(_))}")
    // }
    move
  }
}
