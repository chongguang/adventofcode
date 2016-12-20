import scala.annotation.tailrec
import scala.io.Source

case class Range(from:Long, to:Long)

val stream : String = getClass.getResource("/day20input.txt").getPath
val ranges: List[Range] = Source.fromFile( stream ).getLines.toList.map(s=> {
  val l = s.split('-').toList
  Range(l.head.toLong, l.tail.head.toLong)
})



@tailrec def mergeRecursif(list: List[Range], merged: List[Range] = Nil): List[Range] = list match {
  case x :: y :: rest =>
    if (y.from > x.to + 1) mergeRecursif(y :: rest, merged :+ x)
    else mergeRecursif( Range(x.from, x.to max y.to) :: rest, merged)
  case _ =>
    merged ++ list
}

val sorted = ranges.sortBy(_.from)
def merge(rs: List[Range]): List[Range] = mergeRecursif(rs)

val merged = merge(sorted)

val nb = merged.foldLeft(0L){
  (acc, i) => acc + i.to - i.from + 1
}


val res2 = 4294967295L + 1 - nb