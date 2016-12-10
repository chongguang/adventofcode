import scala.io.Source

val stream : String = getClass.getResource("/day10input.txt").getPath
val instructionStringList = Source.fromFile( stream ).getLines.toList

def giveInstruction(str: String): Map[Int, ((Int, String), (Int, String))] = {
  val list = str.split(' ')
  Map(list(1).toInt -> ((list(6).toInt, list(5)), (list(11).toInt, list(10))))
}

def initInstruction(str: String): (Int, Int) = {
  val list = str.split(' ')
  (list(1).toInt, list(5).toInt)
}

class Bot(id: Int, chips: List[Int]) {
  def Id = id
  def Chips = chips

  def isReadyToGiveChips: Boolean = Chips.length == 2
  def receiveAChip(chip: Int): Bot = new Bot(Id, chip :: Chips )
  def printBot = print("id: " + id + "Chips: " + Chips.toString() + "  ||  ")
  def giveChips: Bot = new Bot(Id, List())
}

class Output(id: Int, chips: List[Int]) {
  def Id = id
  def Chips = chips
  def receiveAChip(chip: Int): Output = new Output(Id, chip :: Chips )
}

val seperatedInsList = instructionStringList.foldLeft((List[(Int, Int)](), Map[Int, ((Int, String), (Int, String))]())){
  (acc, instruction) => {
    instruction.head match {
      case 'v' => ( initInstruction(instruction):: acc._1, acc._2)
      case _ => (acc._1, giveInstruction(instruction) ++ acc._2)
    }
  }
}

val initInsList = seperatedInsList._1
val giveInsMap = seperatedInsList._2
val outputsMap: Map[Int, Output] = Map[Int, Output]()
val botsMap = initInsList.foldLeft(Map[Int, Bot]()){
  (acc, i) => {
    if (acc.exists(_._1 == i._2)) acc + (i._2 -> acc.get(i._2).get.receiveAChip(i._1))
    else acc + (i._2 -> new Bot(i._2, List(i._1)))
  }
}

def findFirstBotWhoIsReadyToGive(bots: Map[Int, Bot]): Bot = {
  bots.toList.filter(_._2.isReadyToGiveChips).head._2
}

def findGiveInsByBot(bot: Bot): ((Int,String), (Int, String)) = giveInsMap.get(bot.Id).get

def giveChipToBot(chip: Int, id: Int, botMap: Map[Int, Bot]): Map[Int, Bot] = {
  if (botMap.contains(id)) botMap + (id -> botMap(id).receiveAChip(chip))
  else botMap + (id -> new Bot(id, List(chip)))
}

def giveChipToOutput(chip: Int, id: Int, outputMap: Map[Int, Output]): Map[Int, Output] = {
  if (outputMap.exists(_._1 == id)) outputMap + (id -> outputMap(id).receiveAChip(chip))
  else outputMap + (id ->  new  Output(id, List(chip)))
}

val giveInsNb = giveInsMap.size

val finalState = (1 to giveInsNb).foldLeft((botsMap, outputsMap)){
  (acc, i) => {
    val botToGive = findFirstBotWhoIsReadyToGive(acc._1)
    val low = botToGive.Chips.sorted.head
    val high = botToGive.Chips.sorted.tail.head
    val giveIns = findGiveInsByBot(botToGive)
    val lowId = giveIns._1._1
    val highId = giveIns._2._1

    if(low == 17 && high == 61) {
      println("==============================")
      println(botToGive.printBot)
      println("==============================")
    }

    val s = giveIns._1._2 + giveIns._2._2
    s match {
      case "botbot" => (giveChipToBot(high, highId, giveChipToBot(low, lowId, acc._1)) + (botToGive.Id -> botToGive.giveChips), acc._2)
      case "outputoutput" => (acc._1 + (botToGive.Id -> botToGive.giveChips), giveChipToOutput(high, highId, giveChipToOutput(low, lowId, acc._2)))
      case "botoutput" => (giveChipToBot(low, lowId, acc._1) + (botToGive.Id -> botToGive.giveChips), giveChipToOutput(high, highId, acc._2))
      case _ => (giveChipToBot(high, highId, acc._1) + (botToGive.Id -> botToGive.giveChips), giveChipToOutput(low, lowId, acc._2))
    }
  }
}

val finalOutputs = finalState._2
println(finalOutputs(0).Chips.toString())
println(finalOutputs(1).Chips.toString())
println(finalOutputs(2).Chips.toString())