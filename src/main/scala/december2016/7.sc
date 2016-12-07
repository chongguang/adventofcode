import scala.io.Source

val stream : String = getClass.getResource("/day7input.txt").getPath
val IPs = Source.fromFile( stream ).getLines.toList


val test = "ozgymklbikxnhme[bbbbemtxaxyxnnazaxm]jszcoclvxluigfgdlq[bkkxgjapnrpvovq]tdsakecfolgpiynztiu"

val test1 = "azefzae"

val brackets = "\\[(.*?)\\]".r

def findFirstHypernetSequences(str: String): Option[String] = {
  (brackets findFirstIn str)
}


def extractContentRecursif(str: String, accOutsideBrackets:List[String], accInsideBrackets:List[String]):
(List[String], List[String]) = findFirstHypernetSequences(str).toString match {
  case "None" => (accOutsideBrackets :+ str, accInsideBrackets)
  case _ => {
    val brac: String = findFirstHypernetSequences(str).get
    val start = str.indexOf(brac)
    val end = start + brac.length
    val outLeft = str.substring(0,start)
    val inside = str.substring(start+1, end-1)
    val outRight = str.substring(end)
    extractContentRecursif(outRight, accOutsideBrackets :+ outLeft, accInsideBrackets :+ inside)
  }
}

def extractContent(str: String): (List[String], List[String]) = {
  def outsideBrackets: List[String] = List()
  def insideBrackets: List[String] = List()
  extractContentRecursif(str, outsideBrackets, insideBrackets)
}

def isABBA(list: List[Char]): Boolean = {
  list(0) == list(3) && list(1) == list(2) && list(0) != list(1)
}

def isABBAString(str: String, acc: Boolean): Boolean = str.length match{
  case len if len < 4 => acc
  case _ => isABBAString(str.substring(1), acc || isABBA(str.substring(0,4).toList))
}

def listContainsABBAString(list: List[String], acc: Boolean): Boolean = list match {
  case Nil => acc
  case _ =>  listContainsABBAString(list.tail,  isABBAString(list.head, false) || acc)
}

def isGoodIP(str: String): Boolean = {
  def lists = extractContent(str)
  listContainsABBAString(lists._1, false) && !listContainsABBAString(lists._2, false)
}

print(isGoodIP("abba[mnop]qrst"))

IPs.foldLeft(0){
  (acc, ip) => if(isGoodIP(ip)) (acc + 1) else acc
}

def isABA(list: List[Char]): Boolean = {
  list(0) == list(2) && list(0) != list(1)
}

def findAllABAsRecursif(str: String, acc: List[String]): List[String] = str.length match {
  case len if len < 3 => acc
  case _ => if (isABA(str.substring(0,3).toList)) findAllABAsRecursif(str.substring(1), acc :+ str.substring(0,3))
  else findAllABAsRecursif(str.substring(1), acc)

}

def findAllABAs(str: String): List[String] = findAllABAsRecursif(str, List())

def isSSLSupported(str: String): Boolean = {
  val lists = extractContent(str)
  val outsideList = lists._1
  val insideList = lists._2
  val outsideABAs = outsideList.foldLeft(List[String]()){
    (acc, s)=> acc ::: findAllABAs(s)
  }
  val insideABAs = insideList.foldLeft(List[String]()){
    (acc, s)=> acc ::: findAllABAs(s)
  }
  val inversedOutsideABAs = outsideABAs.map(x=>{
    val A = x.charAt(0)
    val B = x.charAt(1)
    List(B,A,B).mkString
  })
  inversedOutsideABAs.intersect(insideABAs).length > 0
}

IPs.foldLeft(0){
  (acc, ip) => if (isSSLSupported(ip)) acc +1 else acc
}