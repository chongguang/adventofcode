import scala.io.Source

val stream : String = getClass.getResource("/day2input.txt").getPath
val lines = Source.fromFile( stream ).getLines.toList.map(x=>x.toList)

// Part 1
/*
def canUp( x: Int ): Boolean = x <= 9 && x >= 1 && x > 3
def canDown( x: Int ): Boolean = x <= 9 && x >= 1 &&  x < 7
def canRight(x: Int): Boolean = x <= 9 && x >= 1 && x != 3 && x != 6 && x != 9
def canLeft(x: Int): Boolean = x <= 9 && x >= 1 && x != 1 && x != 4 && x != 7

def moveUp(x: Int): Int = x - 3
def moveDown(x: Int): Int = x + 3
def moveRight(x: Int): Int = x + 1
def moveLeft(x: Int): Int = x - 1
*/

// Part 2
def canUp( x: Int ): Boolean = x <= 13 && x >= 1 && x != 1 && x != 2 && x != 5 && x != 4 && x != 9
def canDown( x: Int ): Boolean = x <= 13 && x >= 1 && x != 5 && x != 10 && x != 12 && x != 13 && x != 9
def canRight(x: Int): Boolean = x <= 13 && x >= 1 &&  x != 1 && x != 4 && x != 9 && x != 12 && x != 13
def canLeft(x: Int): Boolean = x <= 13 && x >= 1 &&  x != 1 && x != 2 && x != 5 && x != 10 && x != 13

def moveUp(x: Int): Int = {
  //println("moving up from " + x)
  x match {
    case 3 => x - 2
    case 13 => x - 2
    case _ => x - 4
  }
}
def moveDown(x: Int): Int = {
  //println("moving down from " + x)
  x match {
    case 1 => x + 2
    case 11 => x + 2
    case _ => x + 4
  }
}
def moveRight(x: Int): Int = {
  //println("moving right from " + x)
  x + 1
}
def moveLeft(x: Int): Int = {
  //println("moving left from " + x)
  x - 1
}
def stay(x: Int): Int = {
  //println("stay at " + x)
  x
}

def moveOneStep(x: Int, direction: Char): Int = direction match {
  case 'U' => if (canUp(x)) moveUp(x) else stay(x)
  case 'D' => if (canDown(x)) moveDown(x)else stay(x)
  case 'R' => if (canRight(x)) moveRight(x) else stay(x)
  case 'L' => if (canLeft(x)) moveLeft(x) else stay(x)
}

def moveOneInstruction(start: Int, steps: List[Char]): Int = {
  val end = steps.foldLeft(start) {
    (position, movement) => {
     // println(movement)
      moveOneStep(position, movement)
    }
  }
  println(end)
  end
}

lines.foldLeft(5) {
  (position, instrunction) => moveOneInstruction(position, instrunction)
}