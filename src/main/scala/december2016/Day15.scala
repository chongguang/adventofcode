package december2016

/**
  * Created by ouyqf on 2016/12/17.
  */
object Day15 {

  def diskPosition(time: Int, original: Int, total: Int): Int = {
    (time + original)%total
  }

  diskPosition(2, 1, 2)

  def disk1(time: Int): Int = diskPosition(time, 1, 17)
  def disk2(time: Int): Int = diskPosition(time, 0, 7)
  def disk3(time: Int): Int = diskPosition(time, 2, 19)
  def disk4(time: Int): Int = diskPosition(time, 0, 5)
  def disk5(time: Int): Int = diskPosition(time, 0, 3)
  def disk6(time: Int): Int = diskPosition(time, 5, 13)
  def disk7(time: Int): Int = diskPosition(time, 0, 11)

  def findFirstQ1(t: Int): Int = {
    if(
        disk1(t+1) == 0 &&
        disk2(t+2) == 0 &&
        disk3(t+3) == 0 &&
        disk4(t+4) == 0 &&
        disk5(t+5) == 0 &&
        disk6(t+6) == 0
    ) t else findFirstQ1(t+1)
  }

  def findFirstQ2(t: Int): Int = {
    if(
      disk1(t+1) == 0 &&
        disk2(t+2) == 0 &&
        disk3(t+3) == 0 &&
        disk4(t+4) == 0 &&
        disk5(t+5) == 0 &&
        disk6(t+6) == 0 &&
        disk7(t+7) == 0
    ) t else findFirstQ2(t+1)
  }

  def main(args: Array[String]): Unit = {
    val res1 = findFirstQ1(0)
    println(res1)
    val res2 = findFirstQ2(0)
    println(res2)
  }

}
