package december2016

import scala.io.Source

/**
  * Created by chongguang on 2016/12/24.
  */
object Day24 {

  val stream : String = getClass.getResource("/day24input.txt").getPath
  val map = Source.fromFile( stream ).getLines.toArray.map(s=>s.toCharArray)

  case class Point(x:Int, y:Int)
  val points = for(
    p <- List('0', '1', '2', '3', '4', '5', '6', '7');
    x <- map(0).indices;
    y <- map.indices
    if map(y)(x) == p
  ) yield {Point(x,y)}

  List(Point(1,1), Point(1,2)).contains(Point(1,2))

  def possibleMove(p: Point, history:List[Point]): List[Point] = {
    val moves = List(Point(p.x, p.y-1), Point(p.x, p.y+1), Point(p.x-1,p.y), Point(p.x+1,p.y))
    moves.filter(m=> map(m.y)(m.x) != '#' && !history.contains(m))
  }

  def shortestPath(from: Point, to: Point, path:List[Point]): Int = {
    if(from == to) {
      //println(path.length - 1)
      path.length - 1
    }
    else {
      //println("from (" + from.x + ", " + from.y + ") to (" + to.x + ", " + to.y + ")" )
      val moves = possibleMove(from, path)
      //println(moves)
      if(moves.isEmpty) Int.MaxValue
      else {
        val paths = for (m <- moves ) yield {
          shortestPath(m, to, path :+ from)
        }
        paths.min
      }
    }
  }


  //shortestPath(points(0), points(1), List(points(0)))




  case class Path(from:Int, to: Int, length: Int)




  def main(args: Array[String]): Unit = {

    val distances = for(
      start <- 0 to 6;
      end <- start+1 to 7
    ) yield Path(start, end, shortestPath(points(start), points(end), List(points(start))))

    println(distances.toString())

  }




}