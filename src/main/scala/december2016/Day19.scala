package december2016

/**
  * Created by ouyqf on 2016/12/19.
  */
object Day19 {

  def removeElementAtEven(l :List[Int]): List[Int] = l.zipWithIndex.filter(p=>p._2%2==0).map{p=>p._1}

  def Question1(l: List[Int]):Int = {
    l.length match {
      case 1 => l.head
      case x if x%2 == 0 => Question1(removeElementAtEven(l))
      case _ => Question1(l.last :: removeElementAtEven(l.dropRight(1)))
    }
  }

  def indiceToRemove(list: List[Int], position: Int): Int = (position + list.length/2) % list.length
  def deleteElementByIndice(list: List[Int], indice: Int): List[Int] = list.take(indice) ++ list.drop(indice+1)
  def newIndice(oldlist: List[Int], oldIndice: Int): Int = (oldIndice + 1) % oldlist.length

  def Question2(list: List[Int], position: Int): Int = {
    list.length match {
      case 1 => list.head
      case _ => {
        val index2remove = indiceToRemove(list, position)
        val newList = deleteElementByIndice(list, index2remove)
        val newPosition = newIndice(list, position)
        Question2(newList, newPosition)
      }
    }
  }


  /*

  Q2 pattern:
  (1,1)

(2,1)
(3,3)

(4,1)
(5,2)
(6,3)
(7,5)
(8,7)
(9,9)

(10,1)
(11,2)
(12,3)
(13,4)
(14,5)
(15,6)
(16,7)
(17,8)
(18,9)
(19,11)
(20,13)
(21,15)
(22,17)
(23,19)
(24,21)
(25,23)
(26,25)
(27,27)

(28,1)
(29,2)
(30,3)
(31,4)
(32,5)
(33,6)
(34,7)
(35,8)
(36,9)
(37,10)
(38,11)
(39,12)
(40,13)
(41,14)
(42,15)
(43,16)
(44,17)
(45,18)
(46,19)
(47,20)
(48,21)
(49,22)
(50,23)
(51,24)
(52,25)
(53,26)
(54,27)
(55,29)
(56,31)
(57,33)
(58,35)
(59,37)
(60,39)
(61,41)
(62,43)
(63,45)
(64,47)
(65,49)
(66,51)
(67,53)
(68,55)
(69,57)
(70,59)
(71,61)
(72,63)
(73,65)
(74,67)
(75,69)
(76,71)
(77,73)
(78,75)
(79,77)
(80,79)
(81,81)


So input=3004953, 3^13 < input < 3^14, answer = input - 3^13 = 1410630
   */


  def main(args: Array[String]): Unit = {
    //val res1 = Question2((1 to 5).toList,0)
    //print(res1)
    //val res2 = Question2((1 to 3004953).toList,0)
    //print(res2)
  }

}
