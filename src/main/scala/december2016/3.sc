import scala.io.Source

val stream : String = getClass.getResource("/day3input.txt").getPath
val triangles = Source.fromFile( stream ).getLines.toList.map(_.trim.replaceAll(" +", " ").split(' ').toList.map(_.toInt))

def isTriangle(sides: List[Int]): Boolean = {
  val a = sides.head
  val b = sides.tail.head
  val c = sides.tail.tail.head
  (a + b > c) && (a + c > b) && (b + c > a)
}

def count(list: List[List[Int]]): Int = {
  list.foldLeft(0) {
    (acc, triangle) => if (isTriangle(triangle)) (acc + 1) else acc
  }
}

count(triangles)


def countVertically(acc: Int, list: List[List[Int]]): Int = list.length match {
  case 0 => acc
  case 1 => acc
  case 2 => acc
  case _ => {
    val splited = list.splitAt(3)
    countVertically(acc + findTrianglesIn3(splited._1), splited._2)
  }
}

def findTrianglesIn3(list: List[List[Int]]): Int = {
  //println(list.toString())
//println("1")
  def line1 = list.head
  //println("2")
  def line2 = list.tail.head
  //println("3")
  def line3 = list.tail.tail.head
  def t1 = line1.head::line2.head::line3.head::Nil
  def t2 = line1.tail.head::line2.tail.head::line3.tail.head::Nil
  def t3 = line1.tail.tail.head::line2.tail.tail.head::line3.tail.tail.head::Nil

  def tList = t1::t2::t3::Nil
  count(tList)

}

countVertically(0, triangles)