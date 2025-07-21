package fr.esgi.al.funprog

object MatrixTest {
  def assertZero(a: Int, msg: String): Unit =
    if (!(a.compare(0) == 0)) sys.error(msg)
  def assertBigInt(a: BigInt, b: BigInt, msg: String): Unit =
    if (!(a.compare(b) == 0)) sys.error(msg)

  def main(args: Array[String]): Unit = {
    val grid = Grid(2, 2)
    val matrix = Matrix.fromGrid(grid)
    val instrs = List(
      Instruction(1, Increment, Color(1,0,0), List((0,0))),
      Instruction(2, Decrement, Color(1,0,0), List((0,0)))
    )
    val resultOpt = Matrix.runInstructionsWithTime(matrix, instrs)
    resultOpt.fold(sys.error("runInstructionsWithTime should return Some")) {
      case (finalMatrix, _) =>
        finalMatrix.grid.headOption.fold(sys.error("Grid should have at least one row")) { row =>
          row.headOption.fold(sys.error("Row should have at least one LED")) { led =>
            assertZero(led.intensity, "LED intensity should be 0 (off)")
            assertBigInt(led.totalOnDuration, BigInt(1), "LED totalOnDuration should be 1")
            println("All Matrix tests passed!")
          }
        }
    }
  }
}
