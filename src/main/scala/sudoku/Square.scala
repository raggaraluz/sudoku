package sudoku

case class Square(value: Option[Int], free: Set[Int], position: (Int, Int)) {
  override def toString() = value match {
    case Some(0) => " "
    case Some(value) => value.toString()
    case None => " "
  }

  def isEmpty = value match {
    case None => true
    case _ => false
  }
}

object Square {
  def apply(position: (Int, Int), max: Int) : Square = {
    Square(None, (1 to max).toSet, position)
  }

  def apply(move: Move) : Square = {
    Square(Some(move.value), Set(), move.pos)
  }
}
