import scala.io.Source

val stream : String = getClass.getResource("/day9input.txt").getPath
val compressed = Source.fromFile( stream ).getLines.toList(0)

val marker = """\(\d+x\d+\)""".r

def findFirstMarker(str: String): Option[String] = (marker findFirstIn str)

def decompressRecursif(acc: Long, compressedString: String): Long = {
  val firstMarker = findFirstMarker(compressedString)
  firstMarker match {
    case None => acc + compressedString.length
    case _ => {
      val markerString = firstMarker.get
      val markerPosition = compressedString.indexOf(markerString)
      if( markerPosition > 0) {
        val newAcc = acc + compressedString.substring(0, markerPosition).length
        val rest = compressedString.substring(markerPosition + markerString.length)
        val params = markerString.substring(1).dropRight(1).split('x').toList
        val length2take = params.head.toInt
        val times = params.tail.head.toInt
        val string2repeat = rest.substring(0, length2take)
        val string4later = rest.substring(length2take)
        newAcc + times * decompress(string2repeat) + decompress(string4later)
      } else {
        val params = markerString.substring(1).dropRight(1).split('x').toList
        val length2take = params.head.toInt
        val times = params.tail.head.toInt
        val string2repeat = compressedString.substring(markerString.length, markerString.length + length2take)
        val string4later = compressedString.substring(markerString.length + length2take)
        acc + times * decompress(string2repeat) + decompress(string4later)
      }
    }
  }
}

def decompress(str: String): Long = decompressRecursif(0, str)

val test1 = "ADVENT"
val test2 = "A(1x5)BC"
val test3 = "(3x3)XYZ"
val test4 = "A(2x2)BCD(2x2)EFG"
val test5 = "(6x1)(1x3)A"
val test6 = "X(8x2)(3x3)ABCY"

val test7 = "(3x3)XYZ"
val test8 = "X(8x2)(3x3)ABCY"
val test9 = "(27x12)(20x12)(13x14)(7x10)(1x12)A"
val test10 = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

decompress(compressed)
