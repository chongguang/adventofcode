//val itinerary = "R3, L2, L2, R4, L1, R2, R3, R4, L2, R4, L2, L5, L1, R5, R2, R2, L1, R4, R1, L5, L3, R4, R3, R1, L1, L5, L4, L2, R5, L3, L4, R3, R1, L3, R1, L3, R3, L4, R2, R5, L190, R2, L3, R47, R4, L3, R78, L1, R3, R190, R4, L3, R4, R2, R5, R3, R4, R3, L1, L4, R3, L4, R1, L4, L5, R3, L3, L4, R1, R2, L4, L3, R3, R3, L2, L5, R1, L4, L1, R5, L5, R1, R5, L4, R2, L2, R1, L5, L4, R4, R4, R3, R2, R3, L1, R4, R5, L2, L5, L4, L1, R4, L4, R4, L4, R1, R5, L1, R1, L5, R5, R1, R1, L3, L1, R4, L1, L4, L4, L3, R1, R4, R1, R1, R2, L5, L2, R4, L1, R3, L5, L2, R5, L4, R5, L5, R3, R4, L3, L3, L2, R2, L5, L5, R3, R4, R3, R4, R3, R1"
val itinerary = "R3, L2, L2, R4, L1, R2, R3, R4, L2, R4, L2, L5, L1, R5, R2, R2, L1, R4, R1, L5, L3, R4, R3, R1, L1, L5, L4, L2, R5, L3, L4, R3, R1, L3, R1, L3, R3, L4, R2, R5, L190, R2, L3, R47, R4, L3, R78, L1, R3, R190, R4, L3, R4, R2, R5, R3, R4, R3, L1, L4, R3, L4, R1, L4, L5, R3, L3, L4, R1, R2, L4, L3, R3, R3, L2, L5, R1, L4, L1, R5, L5, R1, R5, L4, R2, L2, R1, L5, L4, R4, R4, R3, R2, R3, L1, R4, R5, L2, L5, L4, L1, R4, L4, R4, L4, R1, R5, L1, R1, L5, R5, R1, R1, L3, L1, R4, L1, L4, L4, L3, R1, R4, R1, R1, R2, L5, L2, R4, L1, R3, L5, L2, R5, L4, R5, L5, R3, R4, L3, L3, L2, R2, L5, L5, R3, R4, R3, R4, R3, R1"
//val itinerary = "R2, L3"
val itineraryArray = itinerary.split(", ").map(x => new Movement(x.head, x.tail.toInt))

object Direction extends Enumeration {
  type Direction = Value
  val N, S, W, E = Value
}
import Direction._

class Movement(r: Char, s: Int) {
  def rotation = r
  def steps = s
}

class Position(o: Direction, wd: Int, nd: Int) {
  def orientation = o
  def westDistance = wd
  def northDistance = nd

  def move(m: Movement):  Position = {
    val newOri = newOrientation(o, m.rotation)
    newOri match {
      case N => new Position(newOri, wd, nd+m.steps)
      case S => new Position(newOri, wd, nd-m.steps)
      case E => new Position(newOri, wd-m.steps, nd)
      case W => new Position(newOri, wd+m.steps, nd)
    }
  }

  private def newOrientation(old: Direction, rotation: Char): Direction = (old, rotation) match {
    case (N, 'R') => E
    case (N, 'L') => W
    case (E, 'R') => S
    case (E, 'L') => N
    case (S, 'R') => W
    case (S, 'L') => E
    case (W, 'R') => N
    case (W, 'L') => S
    case (_, _) => N
  }

}

val start = new Position(N, 0, 0)
def end = itineraryArray.foldLeft(start) {
  (position, movement) => position.move(movement)
}

println(end.northDistance)
println(end.westDistance)