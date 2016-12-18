package december2016

/**
  * Created by Chongguang on 2016/12/18.
  */
object Day18 {

  val input = "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

  def trapOrSafe(s: String): String = s match {
    case "^^." => "^"
    case ".^^" => "^"
    case "..^" => "^"
    case "^.." => "^"
    case _ => "."
  }

  def newRow(row: String): String = ('.'.+(row) + '.').sliding(3).toList.map(trapOrSafe).mkString

  def constructRows(first: String, nbRows: Int): List[String] = {
    val rows = List(first)
    (1 until nbRows).foldLeft(rows){
      (acc, _) => acc :+ newRow(acc.last)
    }
  }

  def countSafe(rows: List[String]): Int = {
    rows.indices.foldLeft(0){
      (acc, s) => acc + rows(s).count(_ == '.')
    }
  }

  def countSafeOptMem(first: String, nbRows: Int): Int = {
    val start = (first, 0)
    (1 to nbRows).foldLeft(start){
      (acc, _) => {
        (newRow(acc._1), acc._2 + acc._1.count(_ == '.'))
      }
    }._2
  }

  def main(args: Array[String]): Unit = {
    val nbSafe = countSafeOptMem(input, 400000)
    println("NB Safe: " + nbSafe)
  }

}
