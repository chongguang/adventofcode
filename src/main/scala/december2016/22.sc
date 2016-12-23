import scala.io.Source

val stream : String = getClass.getResource("/day22input.txt").getPath
val nodeStrings = Source.fromFile( stream ).getLines.toList

case class Node(x: Int, y:Int, size: Int, used: Int, avail:Int, useage:Int)

def stringToNode(s: String): Node = {
  val list = s.split("""\s+""")
  val coords = list.head.splitAt(16)._2.split("-y")
  Node(
    coords(0).toInt,
    coords(1).toInt,
    list(1).dropRight(1).toInt,
    list(2).dropRight(1).toInt,
    list(3).dropRight(1).toInt,
    list(4).dropRight(1).toInt
  )
}

val nodes = nodeStrings.map(stringToNode)

val nodesSortedByUsed = nodes.groupBy(_.used).toList.sortBy(_._1).drop(1)
val nodesSortedByAvail = nodes.groupBy(_.avail).toList.sortBy(_._1)

def calcule(nodes: (Int, List[Node])): Int = {
  val sum = nodesSortedByAvail.foldLeft(0){
    (acc, g) => if(g._1 >= nodes._1) acc + g._2.length else acc
  }
  val doubled = nodes._2.foldLeft(0){
    (acc, n) => if (n.used <= n.avail && n.used!=0) acc+1 else acc
  }
  sum * nodes._2.length - doubled
}

def Q1():Int = {
  nodesSortedByUsed.foldLeft(0){
    (acc, g) => acc + calcule(g)
  }
}

val Q2:Int = 10+6+(33-7)+5*32