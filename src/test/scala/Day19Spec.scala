import december2016.Day19._
import org.scalatest.{FlatSpec, Matchers}


/**
  * Created by Chongguang on 2016/12/19.
  */
class Day19Spec extends FlatSpec with Matchers {

  "indiceToRemove" should "work" in {
    indiceToRemove(List(1,2,3,4,5),0) shouldBe 2

    indiceToRemove(List(1,2,4,5),1) shouldBe 3

    indiceToRemove(List(1,2,4),2) shouldBe 0
  }

  "deleteElementByIndice" should "work" in {
    deleteElementByIndice(List(1,2,3,4,5),2) shouldBe List(1,2,4,5)

    deleteElementByIndice(List(1,2,4,5),3) shouldBe List(1,2,4)

    deleteElementByIndice(List(1,2,4),1) shouldBe List(1,4)
  }

  "newIndice" should "work" in {
    newIndice(List(1,2,3,4,5), 0) shouldBe 1

    newIndice(List(1,2,4,5), 1) shouldBe 2

    newIndice(List(1,2,4), 2) shouldBe 0
  }
}
