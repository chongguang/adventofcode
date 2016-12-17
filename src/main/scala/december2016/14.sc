import java.security.MessageDigest

def md5(s: String) = {
  MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X" format _).mkString.toLowerCase
}

val input = "jlmsuwbz"
//val input = "abc"

def stretchedHash(s: String):String = {
  (1 to 2017).foldLeft(s){
    (acc, i) => {
      md5(acc)
    }
  }
}


def findRepetitions(s: String, len:Int): List[String] = s.sliding(len).toList.filter(_.distinct.length == 1).distinct

def findFirstTriple(str: String): List[String] = {
  val l = str.sliding(3).toList.filter(_.distinct.length == 1)
  l.length match {
    case 0 => List[String]()
    case _ => List(l.head)
  }
}

class State(keys: List[Int], currentValue: Int, tripleMap: Map[String, List[Int]], fiveOfAKindMap: Map[String, List[Int]]){
  val Keys: List[Int] = keys
  val CurrentValue: Int = currentValue
  val TripleMap: Map[String, List[Int]] = tripleMap
  val FiveOfAKindMap: Map[String, List[Int]] = fiveOfAKindMap
}

def decodeIndice(i: Int): (List[String], List[String]) = {
  val hash = stretchedHash(input + i.toString)
  val firstTriple = findFirstTriple(hash)
  val fiveOfAKind = findRepetitions(hash, 5)
  (firstTriple, fiveOfAKind)
}


def addListToMap(oldMap: Map[String, List[Int]], l: List[String], indice: Int): Map[String, List[Int]] = {
  l.foldLeft(oldMap){
    (acc, i) => addStringToMap(acc, i, indice)
  }
}

def addStringToMap(oldMap: Map[String, List[Int]], s: String, indice: Int): Map[String, List[Int]] = {
  val ol = oldMap.getOrElse(s, List[Int]())
  val nl = indice :: ol
  oldMap + (s -> nl)
}

//addStringToMap(Map("aaa" -> List(1,2)), "aaa", 10)
//addListToMap(Map("aaa" -> List(1,2)), List("aaa", "ccc"), 10)


val InitialMaps: (Map[String, List[Int]], Map[String, List[Int]]) = {
  (0 until 1000).foldLeft((Map[String, List[Int]](), Map[String, List[Int]]())){
    (acc, i) => {
      val decode = decodeIndice(i)
      (addListToMap(acc._1, decode._1, i), addListToMap(acc._2, decode._2,i))
    }
  }
}


def findIndexOf64thKey(state: State):State = {
  if(state.Keys.length == 64) state
  else {
    val newIndice = state.CurrentValue + 1000
    val decodeNewIndice = decodeIndice(newIndice)
    val newTripleMap = addListToMap(state.TripleMap, decodeNewIndice._1, newIndice)
    val newFiveOfAKindMap = addListToMap(state.FiveOfAKindMap, decodeNewIndice._2, newIndice)

    val decodeCurrent = decodeIndice(state.CurrentValue)


    decodeCurrent._1.length match {
      case 0 => findIndexOf64thKey(new State(state.Keys, state.CurrentValue+1, newTripleMap, newFiveOfAKindMap))
      case _ => {
        val char = decodeCurrent._1.head.charAt(0)
        val s = List.fill(5)(char).mkString
        val l = newFiveOfAKindMap.getOrElse(s, List[Int]())
        l.count(i => i > state.CurrentValue && i <= newIndice) match {
          case 0 => findIndexOf64thKey(new State(state.Keys, state.CurrentValue+1, newTripleMap, newFiveOfAKindMap))
          case _ => findIndexOf64thKey(new State( state.CurrentValue :: state.Keys, state.CurrentValue+1, newTripleMap, newFiveOfAKindMap))
        }
      }
    }
  }
}


val originalState: State = new State(List[Int](), 0, InitialMaps._1, InitialMaps._2)

val res = findIndexOf64thKey(originalState).Keys

//decodeIndice(89)


