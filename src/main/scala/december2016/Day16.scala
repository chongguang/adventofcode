package december2016

/**
  * Created by Chongguang on 2016/12/16.
  */
object Day16 {

  def generateData(list: List[Int], len: Int): List[Int] = {
    if(list.length >= len) list.take(len)
    else {
      var b = list.reverse.map(d=>if(d==0) 1 else 0)
      generateData(list ++ (0 :: b), len)
    }
  }

  def checksum(data: List[Int]): List[Int] = {
    if(data.length % 2 == 1) data
    else {
      val cs = data.grouped(2).toList.map(p=>
        p match {
          case List(0,1) => 0
          case List(1,0) => 0
          case _ => 1
        }
      )
      checksum(cs)
    }
  }

  def calcule(input: String, dataSize: Int): List[Int] = {
    val inputList = input.toList.map(d=>d.asDigit)
    val data = generateData(inputList, dataSize)
    checksum(data)
  }

  def main(args: Array[String]): Unit = {
    val res = calcule("00111101111101000", 35651584).mkString
    println(res)
  }



}
