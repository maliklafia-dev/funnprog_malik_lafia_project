package fr.esgi.al.funprog

final case class Color(r: Int, g: Int, b: Int)

object Color {
  val Noir = Color(0, 0, 0)
  val Rouge = Color(1, 0, 0)
  val Vert = Color(0, 1, 0)
  val Bleu = Color(0, 0, 1)
  val Blanc = Color(1, 1, 1)

  val CouleursValides: Set[Color] = Set(Noir, Rouge, Vert, Bleu, Blanc)

  def isValid(color: Color): Boolean = CouleursValides.contains(color)
}

sealed trait Action
case object Increment extends Action
case object Decrement extends Action
case object Switch extends Action

object Action {
  def fromSymbol(sym: String): Option[Action] = sym match {
    case "+" => Some(Increment)
    case "-" => Some(Decrement)
    case "%" => Some(Switch)
    case _   => None
  }
}

final case class Grid(rows: Int, cols: Int)

final case class LED(
    position: (Int, Int),
    color: Color,
    intensity: Int,
    lastOnTime: Option[Int],
    totalOnDuration: Int
)

object LED {
  def emptyAt(pos: (Int, Int)): LED =
    LED(
      position = pos,
      color = Color.Noir,
      intensity = 0,
      lastOnTime = None,
      totalOnDuration = 0
    )
}

final case class Instruction(
    time: Int,
    action: Action,
    color: Color,
    coordinates: List[(Int, Int)]
)
