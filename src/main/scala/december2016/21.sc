import scala.io.Source

val stream : String = getClass.getResource("/day21input.txt").getPath
val transformationStrings = Source.fromFile( stream ).getLines.toList

def swapPosition(x: Int, y:Int): String => String = (s: String ) => {
  val charX = s.charAt(x)
  val charY = s.charAt(y)
  s.zipWithIndex.map(t=>{
    t._2 match {
      case p if p == x => charY
      case p if p == y => charX
      case _ => t._1
    }
  }).mkString
}

def swapLetter(x: Char, y:Char): String => String = (s: String) => swapPosition(s.indexOf(x),s.indexOf(y))(s)

def reversePositions(x: Int, y:Int): String => String = (s: String ) => {
  val before = s.substring(0, x)
  val reversed = s.substring(x,y+1).reverse
  y match {
    case l if l < s.length - 1 => before + reversed + s.substring(y+1)
    case _ => before + reversed
  }
}

def rotateLeft(steps: Int): String => String = (s: String) => {
  val t = s.splitAt(steps)
  t._2 + t._1
}

def rotateRight(steps: Int): String => String = (s: String) => rotateLeft(s.length - steps)(s)

def movePosition(from: Int, to: Int): String => String = (s: String) => {
  val fChar = s.charAt(from)
  val newTo = if (from < to) to + 1 else to
  val newFrom = if (from < to) from else from +1
  val (fst, snd) = s.splitAt(newTo)
  val t = fst + fChar + snd
  t.substring(0,newFrom) + t.substring(newFrom+1)

}

def rotateBased(letter: Char): String => String = (s: String) => {
  val index: Int =s.indexOf(letter)
  val steps = if(index>=4) 2+index else 1+index
  val realStep = steps % s.length
  val t = s.splitAt(s.length - realStep)
  t._2 + t._1
}

/*
Position	Rotate by letter	Inverse
0	rotate right 1	rotate left   1
1	rotate right 2	rotate left   1
2	rotate right 3	rotate right 2
3	rotate right 4	rotate left   2
4	rotate left   2	rotate right 1
5	rotate left   1	rotate left   3
6	no change	no change
7	rotate right 1	rotate right 4
 */

def rotateBasedInverse(c: Char): String => String = (s: String) => {
  val p = s.indexOf(c)
  p match {
    case 0 => rotateLeft(1)(s)
    case 1 => rotateLeft(1)(s)
    case 2 => rotateRight(2)(s)
    case 3 => rotateLeft(2)(s)
    case 4 => rotateRight(1)(s)
    case 5 => rotateLeft(3)(s)
    case 6 => s
    case _ => rotateRight(4)(s)
  }
}

def stringToTranformation(s:String): String => String = {
  val list = s.split(' ')
  val first = list.head
  val second = list.tail.head
  (first, second) match {
    case ("swap", "position") => swapPosition(list(2).toInt, list(5).toInt)
    case ("swap", "letter") => swapLetter(list(2).head, list(5).head)
    case ("move", "position") => movePosition(list(2).toInt, list(5).toInt)
    case ("reverse", "positions") => reversePositions(list(2).toInt, list(4).toInt)
    case ("rotate", "right") => rotateRight(list(2).toInt)
    case ("rotate", "left") => rotateLeft(list(2).toInt)
    case _ => rotateBased(list(6).head)
  }
}

def stringToInversedTransformation(s: String): String => String = {
  val list = s.split(' ')
  val first = list.head
  val second = list.tail.head
  (first, second) match {
    case ("swap", "position") => swapPosition(list(2).toInt, list(5).toInt)
    case ("swap", "letter") => swapLetter(list(2).head, list(5).head)
    case ("move", "position") => movePosition(list(5).toInt, list(2).toInt)
    case ("reverse", "positions") => reversePositions(list(2).toInt, list(4).toInt)
    case ("rotate", "right") => rotateLeft(list(2).toInt)
    case ("rotate", "left") => rotateRight(list(2).toInt)
    case _ => rotateBasedInverse(list(6).head)
  }
}

val inversedTransformations = transformationStrings.map(stringToInversedTransformation)
val transformations = transformationStrings.map(stringToTranformation)

transformations.foldLeft("abcdefgh"){
  (acc, t) => t(acc)
}

inversedTransformations.foldLeft("fbgdceah"){
  (acc, t) => t(acc)
}