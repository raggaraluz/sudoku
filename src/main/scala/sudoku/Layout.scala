package sudoku

trait Layout {
  def posToIdx(position : Position) : Int

  def idxToPos(idx: Int) : Position

  def emptyState : State

  val maxValue : Int

  def printBoard(state: State) : String

  def parse(file: String): List[Move]

  val groups: Seq[Group]

  def groups(square: Square): Seq[Group] = {
    groupsPerSquareIdx(posToIdx(square.position))
  }

  def groups(move: Move): Seq[Group] = {
    groupsPerSquareIdx.getOrElse(posToIdx(move.pos), Seq())
  }

  private lazy val groupsPerSquareIdx: Map[Int, Seq[Group]] = {
    val idxGroup = for {
      group <- groups
      idx <- group
    } yield (idx, group)

    idxGroup.groupBy(_._1).mapValues(_.unzip._2)
  }
}
