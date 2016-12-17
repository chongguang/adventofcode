import java.security.MessageDigest



def md5(s: String): String = {
  MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X" format _).mkString.toLowerCase
}

val passcode = "pxxbnzuo"

def isOpen(char: Char): Boolean = {
  char == 'b' ||
    char == 'c' ||
    char =='d' ||
    char == 'e' ||
    char == 'f'
}

case class Step(x: Int, y:Int, path: String)

def possibleMove(s: Step, code: String): List[Step] = {
  val doorsSate = md5(code + s.path).take(4).toList.map(isOpen)
  val nextSteps = List(
    Step(s.x, s.y-1, s.path :+ 'U'),
    Step(s.x, s.y+1, s.path :+ 'D'),
    Step(s.x-1, s.y, s.path :+ 'L'),
    Step(s.x+1, s.y, s.path :+ 'R')
  )
  (nextSteps zip doorsSate).filter(p=>
      p._1.x >=0 &&
      p._1.x <=3 &&
      p._1.y >=0 &&
      p._1.y <=3 &&
      p._2
  ).map(p=>p._1)
}


possibleMove(Step(0,1,"D"),"hijkl")
possibleMove(Step(0,0,"DU"),"hijkl")
possibleMove(Step(1,0,"DUR"),"hijkl")


def longestPath(current: Step, code: String): Int = {
  if(current.x == 3 && current.y == 3) {
    current.path.length
  }
  else {
    val moves = possibleMove(current, code)
    if(moves.isEmpty) Int.MinValue
    else {
      val paths = for (m <- moves ) yield {
        longestPath(m, code)
      }
      paths.max
    }
  }
}

def shortestPath(current: Step, code: String): Int = {
  if(current.x == 3 && current.y == 3) {
    //println(current.path)
    current.path.length
  }
  else {
    val moves = possibleMove(current, code)
    if(moves.isEmpty) Int.MaxValue
    else {
      val paths = for (m <- moves ) yield {
        shortestPath(m, code)
      }
      paths.min
    }
  }
}



  val sp = shortestPath(Step(0,0,""), passcode)
  val lp = longestPath(Step(0,0,""), passcode)

