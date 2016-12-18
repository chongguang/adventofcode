package december2016

import java.security.MessageDigest
import scala.util.Random

/**
  * Created by Chongguang on 2016/12/17.
  */
object Day17 {

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
    val doorsState = md5(code + s.path).take(4).toList.map(isOpen)
    val nextSteps = List(
      Step(s.x, s.y-1, s.path :+ 'U'),
      Step(s.x, s.y+1, s.path :+ 'D'),
      Step(s.x-1, s.y, s.path :+ 'L'),
      Step(s.x+1, s.y, s.path :+ 'R')
    )
    (nextSteps zip doorsState).filter(p=>
      p._1.x >=0 &&
        p._1.x <=3 &&
        p._1.y >=0 &&
        p._1.y <=3 &&
        p._2
    ).map(p=>p._1)
  }

  val stepMaxPath = Step(0,0,Random.alphanumeric.take(100).mkString)
  val stepMinPath = Step(0,0,"")

  def path(current: Step, code: String, isShortest: Boolean): Step = {
    if(current.x == 3 && current.y == 3) current
    else {
      val moves = possibleMove(current, code)
      if(moves.isEmpty) if (isShortest) stepMaxPath else stepMinPath
      else {
        val paths = for (m <- moves ) yield {
          path(m, code, isShortest)
        }
        if (isShortest) paths.minBy(_.path.length) else paths.maxBy(_.path.length)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val sp = path(Step(0,0,""), passcode, isShortest = true)
    println(sp.path)
    val lp = path(Step(0,0,""), passcode, isShortest = false)
    print(lp.path.length)
  }
}
