import scala.io.Source

val stream : String = getClass.getResource("/day6input.txt").getPath
val lines = Source.fromFile( stream ).getLines.toList.map(x=>x.toList)

val originalMapSet = List.fill(8)(Map[Char, Int]())

val charCountMapSet = lines.foldLeft(originalMapSet){
  (acc, line) => {
    val newMapSetOfLine = line.map(c=> Map(c->1))
    //println(newMapSetOfLine.toString())
    (acc zip newMapSetOfLine).map(tuple=>{
      (tuple._1.toList ++ tuple._2.toList).groupBy ( _._1).map{ case (k,v) => k -> v.map(_._2).sum }
    })
  }
}

// Part 1
//val message = charCountMapSet.map(m => m.maxBy(_._2)._1)
// Part 2
val message = charCountMapSet.map(m => m.minBy(_._2)._1)
