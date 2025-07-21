package fr.esgi.al.funprog

import scala.io.{Codec, Source}
import scala.util.{Failure, Success}
import java.io.File

@main
def Main(): Unit = {
  val pathOfTheFile = "src/main/scala/progfun/data.txt"
  val src = Source.fromFile(new File(pathOfTheFile))(Codec.UTF8)
  val content = src.getLines().toList

  val gridTry = Parser.parseGridSize(content.headOption.getOrElse(""))

  gridTry match {
    case Success(grid) =>
      println(s"Grille : ${grid.rows} lignes x ${grid.cols} colonnes\n")

      val instructions =
        content.drop(1).zipWithIndex.flatMap { case (line, idx) =>
          Parser.parseInstruction(line, grid).toOption
        }

      val initialMatrix = Matrix.fromGrid(grid)
      val result = Matrix.runInstructionsWithTime(initialMatrix, instructions)
      result match {
        case Some((finalMatrix, matricesOverTime)) =>
          val times = instructions.map(_.time)
          val timesSorted = times.distinct.sorted
          printMatricesOverTime(matricesOverTime, timesSorted)
          val finalTime = times match {
            case Nil    => 0
            case h :: t => t.foldLeft(h)((acc, t) => if (t > acc) t else acc)
          }
          println("\nMatrice finale :")
          println(finalMatrix.toString)
          println("\nRésumé d'activité :")
          println(summarizeMatrix(finalMatrix, finalTime))
        case None =>
          println(
            s"Erreur dans l'historique d'évolution : Conflit d'instructions, erreur métier ou règle non respectée !"
          )
      }

    case Failure(error) =>
      println(s"Erreur parsing grille : ${error.getMessage}")
  }

  src.close()
}

def summarizeMatrix(matrix: Matrix, finalTime: Int): String = {
  val leds = matrix.grid.flatten
  val colorCounts =
    leds.filter(_.intensity == 1).groupBy(_.color).view.mapValues(_.size).toMap
  val cumul = leds.map { led =>
    if (led.intensity == 1) {
      led.totalOnDuration + (finalTime - led.lastOnTime.getOrElse(0))
    } else {
      led.totalOnDuration
    }
  }.sum

  val sortedColors = colorCounts.toList.sortBy { case (color, _) =>
    (color.r, color.g, color.b)
  }
  val colorsStr = sortedColors
    .map { case (color, count) =>
      s"  - (${color.r},${color.g},${color.b}): $count"
    }
    .mkString("\n")

  s"""- taille: ${matrix.rows}x${matrix.cols}
     |- couleurs:
     |$colorsStr
     |- cumul: $cumul""".stripMargin
}

def printMatricesOverTime(matrices: List[Matrix], times: List[Int]): Unit = {
  matrices.zip(times).foreach { case (matrix, time) =>
    println(s"État à t = $time")
    println(matrix.toString)
    println()
  }
}
