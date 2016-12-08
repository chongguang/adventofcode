import scala.io.Source


def parser(str: String): (String, Int, Int) ={
  val list = str.split(" ")
  list(0) match {
    case "rect" => {
      val AB = list(1).split("x")
      ("rect", AB(0).toInt, AB(1).toInt)
    }
    case _ => {
      list(1) match {
        case "row" => {
          val row = list(2).substring(2).toInt
          val steps = list(4).toInt
          ("row", row, steps)
        }
        case _ => {
          val column = list(2).substring(2).toInt
          val steps = list(4).toInt
          ("column", column, steps)
        }
      }
    }
  }
}


val stream : String = getClass.getResource("/day8input.txt").getPath
val instructions = Source.fromFile( stream ).getLines.toList.map(i=>parser(i))

val originalScreen = Array.fill(6, 50)(0)


def reverse(p: Int): Int = if (p==1) 0 else 1

def rect(matrix: Array[Array[Int]], A: Int, B: Int): Array[Array[Int]] = {
  for (i <- 0 until B) {
    for (j <- 0 until A) {
      matrix(i)(j) = 1
    }
  }
  matrix
}

def shiftRightBy1(matrix: Array[Array[Int]], row:Int): Array[Array[Int]] = {
  val last = matrix(row)(matrix(0).length-1)
  for(i <- matrix(0).length -1 until 0 by -1) {
    matrix(row)(i) = matrix(row)(i-1)
  }
  matrix(row)(0) = last
  matrix
}

def rotateRow(matrix: Array[Array[Int]], row:Int, steps: Int): Array[Array[Int]] = {
  val newMatrix = (1 to steps).foldLeft(matrix){
    (acc, n) => {
      shiftRightBy1(acc, row)
    }
  }
  newMatrix
}

def shiftDownBy1(matrix: Array[Array[Int]], column: Int): Array[Array[Int]] = {
  val last = matrix(matrix.length-1)(column)
  for(i <- matrix.length-1 to 1 by -1) {
    matrix(i)(column) = matrix(i-1)(column)
  }
  matrix(0)(column) = last
  matrix
}

def rotateColumn(matrix: Array[Array[Int]], column:Int, steps: Int): Array[Array[Int]] = {
  val newMatrix = (1 to steps).foldLeft(matrix){
    (acc, n) => shiftDownBy1(acc, column)
  }
  newMatrix
}


val testInstructions = List(("rect",3,2), ("column",1,1), ("row",0,4), ("column",1,1),("row", 1,10))


val testOriginalScreen = Array.fill(3, 7)(0)

val newScreen = instructions.foldLeft(originalScreen){
  (acc, i) => {
    i._1 match {
      case "rect" => {
        val res = rect(acc, i._2, i._3)
        res
      }
      case "row" => {
        val res = rotateRow(acc, i._2, i._3)
        res
      }
      case _ => {
        val res = rotateColumn(acc, i._2, i._3)
        res
      }
    }
  }
}



def printMatrix(matrix: Array[Array[Int]]) = {
  for(i <- 0 until matrix.length) {
    println(matrix(i).mkString)
  }
  println("====================================")
}

def sumMatrix(matrix: Array[Array[Int]]): Int = {
  matrix.map(row => row.sum).sum
}

printMatrix(newScreen)

sumMatrix(newScreen)

/*
10010   11100   01100   00110   11110   10000   11100   01100   11110   11110
10010   10010   10010   00010   10000   10000   10010   10010   10000   00010
10010   10010   10010   00010   11100   10000   11100   10000   11100   00100
10010   11100   10010   00010   10000   10000   10010   10000   10000   01000
10010   10000   10010   10010   10000   10000   10010   10010   10000   10000
01100   10000   01100   01100   10000   11110   11100   01100   11110   11110
 */

