import java.security.MessageDigest

def md5(s: String) = {
  MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X" format _).mkString
}

val input = "cxdnnyjw"

val h = md5("abc3231929")

println(isGoodHash(h))

def isGoodHash(s: String): Boolean = {
  s.substring(0,5) == "00000"
}

//Part 1
/*
val hashList = (1999999 to 8300000).toList.map(p=>md5(input + p.toString))

val goodHashList = hashList.filter(p=>isGoodHash(p))

val hashes = List(
  "00000FE1E92080B9951B053E70E31FCB",
  "000007C827126C81FA664211693F2540",
  "000007880153F1B804481A39A6D2E86A",
  "00000A6E225253B6AAA8F20EFBAAD8B5",
  "00000096164643E2E0FBF5A91BFD7F06",
  "00000E77A8B223B2D149990FB634BD74",
  "000006EC13BC03B597BEEE4FA9352176",
  "00000EE477915A03C15F93AC53769648",
  "000009DF43B67685AFFEC89F91B75415",
  "00000426F1CD2F19A38114170B33C5EF")
*/

// Part 2
val hashList = (23000000 to 33000000).toList.foldLeft(Map[Int, Char]()){
  (acc, nb) => {
    val hash = md5(input + nb.toString)
    if(isGoodHash(hash)) {
      val position = hash.charAt(5)
      val code =hash.charAt(6)
      if(position >= '0' && position <= '7' && !acc.exists(_._1 == position.asDigit)) {
        val newAcc = acc ++ Map(position.asDigit -> code)
        println(newAcc.toString())
        newAcc
      }else acc
    } else acc
  }
}