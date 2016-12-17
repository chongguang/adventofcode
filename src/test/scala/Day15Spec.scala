import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Chongguang on 2016/12/17.
  */

import december2016.Day15._

class Day15Spec extends FlatSpec with Matchers {

  "disks" should "be at correct positions" in {
    disk1(0) shouldBe 1
    disk1(1) shouldBe 2
    disk1(16) shouldBe 0

    disk2(0) shouldBe 0
    disk2(1) shouldBe 1
    disk2(7) shouldBe 0

    disk3(0) shouldBe 2
    disk3(1) shouldBe 3
    disk3(17) shouldBe 0

    disk4(0) shouldBe 0
    disk4(1) shouldBe 1
    disk4(5) shouldBe 0

    disk5(0) shouldBe 0
    disk5(1) shouldBe 1
    disk5(3) shouldBe 0
  }

  "question 1 result" should "equal to 317371" in {
    findFirstQ1(0) shouldBe 317371
  }

  "question 2 result" should "equal to 2080951" in {
    findFirstQ2(0) shouldBe 2080951
  }

}
