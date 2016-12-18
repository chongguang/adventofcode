
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Chongguang on 2016/12/18.
  */
import december2016.Day18._

class Day18Spec extends FlatSpec with Matchers {

  "isOpen: " should "reture ture for b, c, d, e, or f and false for others" in {
    newRow("..^^.") shouldBe ".^^^^"
    newRow(".^^^..^.^^") shouldBe "^^.^^^..^^"
  }

  "constructRows" should "work" in {
    val rows1 = constructRows("..^^.", 3)
    rows1(0) shouldBe "..^^."
    rows1(1) shouldBe ".^^^^"
    rows1(2) shouldBe "^^..^"

    val rows2 = constructRows(".^^.^.^^^^", 10)
    rows2(0) shouldBe ".^^.^.^^^^"
    rows2(1) shouldBe "^^^...^..^"
    rows2(2) shouldBe "^.^^.^.^^."
    rows2(3) shouldBe "..^^...^^^"
    rows2(4) shouldBe ".^^^^.^^.^"
  }

  "countSafe" should "work" in {
    val rows1 = constructRows("..^^.", 3)
    countSafe(rows1) shouldBe 6

    val rows2 = constructRows(".^^.^.^^^^", 3)
    countSafe(rows2) shouldBe 12
  }

  "countSafeOptMem" should "work" in {
    countSafeOptMem("..^^.", 3) shouldBe 6

    countSafeOptMem(".^^.^.^^^^", 3) shouldBe 12
  }

}


/*
..^^.
.^^^^
^^..^
 */


/*
.^^.^.^^^^
^^^...^..^
^.^^.^.^^.
..^^...^^^
.^^^^.^^.^
^^..^.^^..
^^^^..^^^.
^..^^^^.^^
.^^^..^.^^
^^.^^^..^^
 */