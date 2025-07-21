package fr.esgi.al.funprog

final case class Matrix(grid: Vector[Vector[LED]]) {
  def rows: Int = grid.length
  def cols: Int = grid.headOption.map(_.length).getOrElse(0)

  def get(x: Int, y: Int): Option[LED] =
    grid.lift(x).flatMap(_.lift(y))

  override def toString: String = {
    grid
      .map { row =>
        row
          .map { led =>
            if (led.intensity == 1) {
              s"(${led.color.r},${led.color.g},${led.color.b})"
            } else {
              "(0,0,0)"
            }
          }
          .mkString(" ")
      }
      .mkString("\n")
  }
}

object Matrix {

  def colorEquals(a: Color, b: Color): Boolean =
    (a.r == b.r) && (a.g == b.g) && (a.b == b.b)

  def empty(rows: Int, cols: Int): Matrix = {
    val grid = Vector.tabulate(rows, cols) { (i, j) => LED.emptyAt((i, j)) }
    Matrix(grid)
  }

  def fromGrid(grid: Grid): Matrix = empty(grid.rows, grid.cols)

  def applyInstructionsAtTime(
      matrix: Matrix,
      instructions: List[Instruction],
      time: Int): Option[Matrix] = {
    val allCoords = instructions.flatMap(_.coordinates)
    val hasDuplicates =
      allCoords.groupBy(identity).exists { case (_, list) => list.sizeIs > 1 }
    if (hasDuplicates) {
      None
    } else {
      val instrMap = instructions
        .flatMap(instr => instr.coordinates.map(coord => coord -> instr))
        .toMap

      val updatedRowsOpt = matrix.grid.map { row =>
        val newRowOpt = row.foldLeft(Option(Vector.empty[LED])) {
          (maybeAcc, led) =>
            maybeAcc.flatMap { acc =>
              instrMap.get(led.position) match {
                case Some(instr) =>
                  updateLED(led, instr.action, instr.color, time).map(newLed =>
                    acc :+ newLed
                  )
                case None =>
                  Some(acc :+ led)
              }
            }
        }
        newRowOpt
      }

      if (updatedRowsOpt.exists(_.isEmpty)) {
        None
      } else {
        val newGrid = updatedRowsOpt.flatten
        if (newGrid.sizeIs != matrix.grid.size) None else Some(Matrix(newGrid))
      }
    }
  }

  private def updateLED(
      led: LED,
      action: Action,
      instrColor: Color,
      time: Int): Option[LED] = {
    action match {
      case Increment =>
        if (led.intensity == 0) {
          Some(
            led.copy(
              color = instrColor,
              intensity = 1,
              lastOnTime = Some(time)
            )
          )
        } else if (colorEquals(led.color, instrColor)) {
          Some(led)
        } else {
          None
        }

      case Decrement =>
        if (led.intensity == 1) {
          val tOn = led.lastOnTime.getOrElse(0)
          Some(
            led.copy(
              intensity = 0,
              totalOnDuration = led.totalOnDuration + (time - tOn),
              lastOnTime = None
            )
          )
        } else {
          Some(led)
        }

      case Switch =>
        if (!colorEquals(led.color, instrColor)) {
          None
        } else {
          if (led.intensity == 0) {
            Some(
              led.copy(
                intensity = 1,
                lastOnTime = Some(time)
              )
            )
          } else {
            val tOn = led.lastOnTime.getOrElse(0)
            Some(
              led.copy(
                intensity = 0,
                totalOnDuration = led.totalOnDuration + (time - tOn),
                lastOnTime = None
              )
            )
          }
        }
    }
  }

  // Applique toutes les instructions dans l'ordre du temps (None si erreur à un instant)
  def runInstructionsWithTime(
      initialMatrix: Matrix,
      instructions: List[Instruction]): Option[(Matrix, List[Matrix])] = {
    val grouped = instructions.groupBy(_.time).toList.sortBy(_._1)
    grouped
      .foldLeft(Option((initialMatrix, List.empty[Matrix]))) {
        case (Some((currentMatrix, acc)), (time, instrsAtTime)) =>
          applyInstructionsAtTime(currentMatrix, instrsAtTime, time).map {
            newMatrix =>
              (newMatrix, newMatrix :: acc)
          }
        case (None, _) => None
      }
      .map { case (finalMatrix, states) => (finalMatrix, states.reverse) }
  }
}
