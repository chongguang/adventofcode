def generateData(list: List[Int], len: Int): List[Int] = {
  if(list.length >= len) {
    list.take(len)
  }
  else {
    var b = list.reverse.map(d=>if(d==0) 1 else 0)
    generateData(list ++ (0 :: b), len)
  }
}


def checksumRecursif(data: List[Int], acc : List[Int] = List[Int]()):List[Int] = {
  if(data.length == 0) acc
  else {
    val pair = data.take(2)
    if(pair(0) == pair(1)) checksumRecursif(data.drop(2), acc :+ 1)
    else checksumRecursif(data.drop(2), acc :+ 0)
  }
}

def checksum(data: List[Int]): List[Int] = {
  if(data.length % 2 == 1) data
  else checksum(checksumRecursif(data))
}




def calcule(input: String, dataSize: Int): List[Int] = {
  val inputList = input.toList.map(d=>d.asDigit)
  println(inputList)
  val data = generateData(inputList, dataSize)
  println(data)
  checksum(data)
}

val res = calcule("00111101111101000", 35651584).mkString
println(res)




