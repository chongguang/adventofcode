import scala.io.Source

val stream : String = getClass.getResource("/day9input.txt").getPath
val compressed = Source.fromFile( stream ).getLines.toList(0)

val marker = """\(\d+x\d+\)""".r

def findFirstMarker(str: String): Option[String] = (marker findFirstIn str)

def decompressRecursif(acc: String, compressedString: String): String = {
  val firstMarker = findFirstMarker(compressedString)
  firstMarker match {
    case None => (acc concat compressedString)
    case _ => {
      val markerString = firstMarker.get
      val markerPosition = compressedString.indexOf(markerString)
      if( markerPosition > 0) {
        val newAcc = acc concat compressedString.substring(0, markerPosition)
        val rest = compressedString.substring(markerPosition + markerString.length)
        val params = markerString.substring(1).dropRight(1).split('x').toList
        val length2take = params.head.toInt
        val times = params.tail.head.toInt
        val string2repeat = rest.substring(0, length2take)
        val string4later = rest.substring(length2take)
        val repeatedString = (1 to times).foldLeft(newAcc){
          (accumulator, n) => {
            accumulator concat string2repeat
          }
        }
        decompressRecursif(repeatedString, string4later)
      } else {
        val params = markerString.substring(1).dropRight(1).split('x').toList
        val length2take = params.head.toInt
        val times = params.tail.head.toInt
        val string2repeat = compressedString.substring(markerString.length, markerString.length + length2take)
        val string4later = compressedString.substring(markerString.length + length2take)
        val repeatedString = (1 to times).foldLeft(acc){
          (accumulator, n) => {
            accumulator concat string2repeat
          }
        }
        decompressRecursif(repeatedString, string4later)
      }
    }
  }
}

def decompress(str: String): String = decompressRecursif("", str)

val test1 = "ADVENT"
val test2 = "A(1x5)BC"
val test3 = "(3x3)XYZ"
val test4 = "A(2x2)BCD(2x2)EFG"
val test5 = "(6x1)(1x3)A"
val test6 = "X(8x2)(3x3)ABCY"
//decompress(compressed).length



var len = compressed.length
var oldStr = compressed
var newLen = 0
var newStr = ""
do{
  newStr = decompress(oldStr)
  newLen = newStr.length
  oldStr = newStr
  len = newLen
  println(newLen)
} while (len != newLen)
