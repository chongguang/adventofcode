import scala.io.Source


val stream : String = getClass.getResource("/day4input.txt").getPath
val rooms = Source.fromFile( stream ).getLines.toList.map(x=>{
  x.split('[').toList
}).map(y=>{
  val t = y.head.split('-').toList
  val name = t.dropRight(1).foldLeft(""){
    (acc, str) => {
      acc + str + "-"
    }
  }

  (t.last.toInt, t.dropRight(1), y.tail.head.dropRight(1), name.dropRight(1) )
})

val roomsWithStringMerged = rooms.map(room => {
  val mergedString = room._2.foldLeft(""){
    (acc, str) => acc.concat(str)
  }
  (room._1, mergedString, room._3, room._4)
})

val roomsWithMapFreq = roomsWithStringMerged.map(
  room => (room._1, room._2.groupBy(_.toChar).map{ p => (p._1, p._2.length)}, room._3, room._4)
)

val groupedByFreq = roomsWithMapFreq.map(
  room => (room._1, room._2.groupBy(_._2).map{ p => (p._1, p._2.keySet.toList.sortWith(_ < _).mkString)}, room._3, room._4)
)


val sortedByFreq = groupedByFreq.map(
  room => (room._1, room._2.toSeq.sortBy(-_._1).toList, room._3, room._4)
)

val mergeChars = sortedByFreq.map(
  room => {
    val mergedString = room._2.foldLeft(""){
      (acc, t) => acc + t._2
    }
    (room._1, mergedString, room._3, room._4)
  }
)

println(mergeChars.length)

def verifyRoom(tuple: (Int, String, String, String)): Boolean = {
  tuple._2.substring(0, 5) == tuple._3
}

val realRooms = mergeChars.filter(verifyRoom)

// Part 1
realRooms.foldLeft(0){
  (acc, t) =>acc + t._1
}



// Part 2
def decryptChar(c: Char, steps: Int): Char = c match {
  case '-' =>  ' '
  case 'z' =>  'a'
  case 'y' =>  'x'
  case _ => (((c.toInt - 97 + 1 + steps) % 26) + 97 -1).toChar
}

def decryptName(sectorID: Int, code: String): String = {
  val steps = sectorID % 26
  val newName = code.map(c=>{
    decryptChar(c, steps)
  })
  newName
}


realRooms.map(room => {
  val realName = decryptName(room._1, room._4)
  if(realName.contains("north") && realName.contains("pole")) println(room.toString + "   " + realName)
})