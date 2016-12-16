/**
  * Created by Chongguang on 2016/12/16.
  */

import org.scalatest._
import december2016.Day16._

class Day10Spec extends FlatSpec with Matchers {
  "generateData: 1" should "become 100" in {
    generateData(List(1),3) === List(1,0,0)
  }

  "generateData: 0" should "become 001" in {
    generateData(List(0),3) === List(0,0,1)
  }

  "generateData: 11111" should "become 11111000000" in {
    generateData(List(1,1,1,1,1),11) === List(1,1,1,1,1,0,0,0,0,0,0)
  }

  "generateData: 111100001010" should "become 1111000010100101011110000" in {
    generateData(List(1,1,1,1,0,0,0,0,1,0,1,0),25) === List(1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,0,0,0,0)
  }

  "checksum: 110010110100" should "become 100" in {
    checksum(List(1,1,0,0,1,0,1,1,0,1,0,0)) === List(1,0,0)
  }

  "calcule: with initial 10000 and size 20" should "become 01100" in {
    calcule("10000", 20) === "01100"
  }

  "calcule: with initial 00111101111101000 and size 272" should "become 01100" in {
    calcule("00111101111101000", 272) === "10011010010010010"
  }

  "calcule: with initial 00111101111101000 and size 35651584" should "become 01100" in {
    calcule("00111101111101000", 35651584) === "10101011110100011"
  }
}
