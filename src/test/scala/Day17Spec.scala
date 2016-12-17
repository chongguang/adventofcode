import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Chongguang on 2016/12/17.
  */


import december2016.Day17._

class Day17Spec extends FlatSpec with Matchers {

  "isOpen: " should "reture ture for b, c, d, e, or f and false for others" in {
    isOpen('b') shouldBe true
    isOpen('c') shouldBe true
    isOpen('d') shouldBe true
    isOpen('e') shouldBe true
    isOpen('f') shouldBe true
    isOpen('a') shouldBe false
    isOpen('9') shouldBe false
  }

  "possible moves" should "work :)" in {
    val ms = possibleMove(Step(0,0,""),"hijkl")
    ms.length shouldBe 1
    ms.head.x shouldBe 0
    ms.head.y shouldBe 1
    ms.head.path shouldBe "D"


    possibleMove(Step(0,1,"D"),"hijkl").length shouldBe 2
    possibleMove(Step(0,0,"DU"),"hijkl").length shouldBe 1
    possibleMove(Step(1,0,"DUR"),"hijkl").length shouldBe 0

  }

  "shortest path" should "work :)" in {
    shortestPath(Step(0,0,""), passcode) shouldBe 10
  }

  "longest path" should "work :)" in {
    longestPath(Step(0,0,""), passcode) shouldBe 752
  }


}
