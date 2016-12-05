import scala.io.Source

val stream : String = getClass.getResource("/day2input.txt").getPath
val lines = Source.fromFile( stream ).getLines.toList.map(x=>x.toList)

def canUp( x: Int ): Boolean = x <= 9 && x >= 1 && x > 3

def canDown( x: Int ): Boolean = x <= 9 && x >= 1 &&  x < 7

def canRight(x: Int): Boolean = x <= 9 && x >= 1 && x != 3 && x != 6 && x != 9

def canLeft(x: Int): Boolean = x <= 9 && x >= 1 && x != 1 && x != 4 && x != 7

def moveOneStep(x: Int, direction: Char): Int = direction match {
  case 'U' => if (canUp(x)) x - 3 else x
  case 'D' => if (canDown(x)) x + 3 else x
  case 'R' => if (canRight(x)) x + 1 else x
  case 'L' => if (canLeft(x)) x - 1 else x
}

def moveOneInstruction(start: Int, steps: List[Char]): Int = {
  val end = steps.foldLeft(start) {
    (position, movement) => moveOneStep(position, movement)
  }
  println(end)
  end
}

lines.foldLeft(5) {
  (position, instrunction) => moveOneInstruction(position, instrunction)
}