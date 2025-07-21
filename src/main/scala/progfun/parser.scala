package fr.esgi.al.funprog

import scala.language.unsafeNulls
import scala.util.{Failure, Success, Try}

object Parser {

  private def failWithException[T](msg: String): Try[T] =
    Failure[T](new Exception(msg))

  /*
   * Prend en parametre la chaine "R,G,B"
   * Valide que les couleur sont bien formée
   * Convertir la chaine en un obj Color
   */
  def parseColor(part: String): Try[Color] = {
    Option(part).map(_.trim) match {
      case Some(nonEmpty) =>
        val components = nonEmpty.nn.split(",").nn.map(_.nn.trim).toList
        components match {
          case rStr :: gStr :: bStr :: Nil =>
            for {
              r <- Try(rStr.toInt)
              g <- Try(gStr.toInt)
              b <- Try(bStr.toInt)
              color = Color(r, g, b)
              _ <-
                if (Color.isValid(color)) Success(())
                else failWithException[Color](s"Couleur invalide: $color")
            } yield color

          case _ =>
            failWithException[Color](s"Format de couleur invalide:  $part")
        }
      case None => failWithException[Color]("Couleur null")
    }
  }

  /*
   * Prend en parametre une Grid ainsi que une String( coordonées "0,0 - 1,2")
   * convertit les coordonnées en une liste de points valides dans une grid
   */
  def parseCoordinates(part: String, grid: Grid): Try[List[(Int, Int)]] = {
    Option(part).map(_.trim) match {
      case Some(p) =>
        val segments = p.nn.split("-").nn.map(_.nn.trim).toList
        segments match {
          case single :: Nil =>
            parseSingleCoord(single, grid).map(List(_))
          case fromStr :: toStr :: Nil =>
            for {
              from <- parseSingleCoord(fromStr, grid)
              to   <- parseSingleCoord(toStr, grid)
            } yield {
              val xMin = math.min(from._1, to._1)
              val xMax = math.max(from._1, to._1)
              val yMin = math.min(from._2, to._2)
              val yMax = math.max(from._2, to._2)
              (for {
                x <- xMin to xMax
                y <- yMin to yMax
              } yield (x, y)).toList
            }
          case _ =>
            failWithException[List[(Int, Int)]](
              "Format de coordonnées invalide: " + part
            )
        }
      case None => failWithException[List[(Int, Int)]]("Coordonnées null")
    }
  }

  /*
   * Prend en parametre une Grid ainsi que les coordonnées x,y
   * Separe le x et y et les verifie si ils sont dans les limote de la Grid
   * renvoie (x,y)
   */
  private def parseSingleCoord(str: String, grid: Grid): Try[(Int, Int)] = {
    val parts = str.nn.split(",").nn.map(_.nn.trim).toList
    parts match {
      case xStr :: yStr :: Nil =>
        for {
          x <- Try(xStr.toInt)
          y <- Try(yStr.toInt)
          _ <-
            if (x >= 0 && x < grid.rows && y >= 0 && y < grid.cols)
              Success(())
            else
              failWithException[(Int, Int)](
                "Coordonnée (" + x.toString + "," + y.toString + ") hors grille " + grid.rows.toString + "x" + grid.cols.toString
              )
        } yield (x, y)
      case _ =>
        failWithException[(Int, Int)]("Format coordonnée invalide: " + str)
    }
  }

  /*
   * Prend en param la ligne entiere et une Grid
   * elle renvoie le temps, l'action, couleur et les coordonnées
   */
  def parseInstruction(line: String, grid: Grid): Try[Instruction] = {
    Option(line).map(_.trim) match {
      case Some(nonEmpty) =>
        val tokens = nonEmpty.nn.split("\\|").nn.map(_.nn.trim).toList
        tokens match {
          case timeStr :: actionStr :: colorStr :: coordsStr :: Nil =>
            for {
              time <- Try(timeStr.toInt)
              action <- Action
                .fromSymbol(actionStr)
                .fold[Try[Action]](
                  failWithException[Action]("Action invalide: " + actionStr)
                )(Success(_))
              color  <- parseColor(colorStr)
              coords <- parseCoordinates(coordsStr, grid)
            } yield Instruction(time, action, color, coords)
          case _ =>
            failWithException[Instruction]("Instruction mal formatée: " + line)
        }
      case None => failWithException[Instruction]("Instruction null")
    }
  }

  /*
   * prend en param une chaîne "n m" et la converti en un objet Grid(n, m).
   * Elle renvoie un erreur si l’entrée est vide, mal formée ou non convertible en entier.
   */
  def parseGridSize(input: String): Try[Grid] = {
    Option(input).map(_.trim) match {
      case Some(nonEmpty) =>
        val parts = nonEmpty.nn.split(" ").nn.map(_.nn.trim).toList
        parts match {
          case rowStr :: colStr :: Nil =>
            for {
              rows <- Try(rowStr.toInt)
              cols <- Try(colStr.toInt)
            } yield Grid(rows, cols)
          case _ =>
            failWithException[Grid]("Format de taille invalide: " + input)
        }
      case None => failWithException[Grid]("Ligne Obligatoire ( null )")
    }
  }
}
