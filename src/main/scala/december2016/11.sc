/*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Floor(chips: List[String], generators: List[String]) {
  def Chips = chips
  def Generators = generators

  override def toString: String = Chips.mkString + "==" + Generators.mkString

  def isSafe: Boolean = Chips.isEmpty || Generators.isEmpty || Chips.intersect(Generators).length == Chips.length

  def give(move: (List[String], List[String])): Floor = new Floor(Chips diff move._1, Generators diff move._2)

  def receive(move: (List[String], List[String])): Floor = new Floor(Chips ++ move._1, Generators ++ move._2)

  def isEmpty: Boolean = Chips.isEmpty && Generators.isEmpty

  def allPossibleMoves: List[(List[String], List[String])] = {
    def simple(list: List[String]): List[List[String]] = list.map(s => List(s))
    def double(list: List[String]): List[List[String]] = list.toSet[String].subsets.map(_.toList).toList.filter(_.length == 2)
    def combo(chips: List[String], generators: List[String]): List[(List[String], List[String])] = chips.intersect(generators).map(s => (List(s), List(s)))

    val simpleChips = simple(Chips)
    val simpleGenerators = simple(Generators)
    val doubleChips = double(Chips)
    val doubleGenerators = double(Generators)

    val movesWithSimpleChips = simpleChips zip List.fill(simpleChips.length)(List[String]())
    val movesWithDoubleChips = doubleChips zip List.fill(doubleChips.length)(List[String]())
    val movesWithSimpleGenerators = List.fill(simpleGenerators.length)(List[String]()) zip simpleGenerators
    val movesWithDoubleGenerators = List.fill(doubleGenerators.length)(List[String]()) zip doubleGenerators

    val movesWithCombos = combo(Chips, Generators)

    movesWithSimpleChips ++ movesWithDoubleChips ++ movesWithSimpleGenerators ++ movesWithDoubleGenerators ++ movesWithCombos
  }
}


class State(floors: List[Floor], steps: Int, floorNb: Int) {
  def Floors: List[Floor] = floors
  def Steps:Int = steps
  def ActualFloor: Int = floorNb

  override def toString: String = Floors.foldLeft(""){
    (acc, f) => acc + f.toString + "--"
  }

  def isStateSafe: Boolean = Floors(0).isSafe && Floors(1).isSafe && Floors(2).isSafe && Floors(3).isSafe

  def isFinalState: Boolean = Floors(0).isEmpty && Floors(1).isEmpty && Floors(2).isEmpty

  def allMoves: List[(List[String], List[String])] = Floors(ActualFloor).allPossibleMoves

  def print = {
    println("----------------------------------------------------------")
    println("F4: " + Floors(3).Chips.toString() + "   " + Floors(3).Generators.toString())
    println("F3: " + Floors(2).Chips.toString() + "   " + Floors(2).Generators.toString())
    println("F2: " + Floors(1).Chips.toString() + "   " + Floors(1).Generators.toString())
    println("F1: " + Floors(0).Chips.toString() + "   " + Floors(0).Generators.toString())
    println("----------------------------------------------------------")
  }

  def nextStates: List[State] = {
    if(isStateSafe) {
      val from = ActualFloor
      val to = List(from+1, from -1).filter(f => {f >= 0 && f <= 3})
      val moves = allMoves
      val allStates = to.flatMap( t=> {
        moves.map(m=>{
          val newFloors = (0 to 3 ).foldLeft(List[Floor]()){
            (acc, i) => {
              if(i != from && i!=t) acc :+ Floors(i)
              else if (i == from ) acc :+ Floors(i).give(m)
              else acc :+ Floors(i).receive(m)
            }
          }
          val newState = new State(newFloors, Steps +1, t)
          newState
        })
      })
      allStates.filter(_.isStateSafe)
    } else List[State]()
  }

}
/*
The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
The third floor contains a thulium-compatible microchip.
The fourth floor contains nothing relevant.
 */



val f0 = new Floor(List("S", "P"), List("S", "P"))
val f1 = new Floor(List("R", "C"), List("T", "R", "C"))
val f2 = new Floor(List("T"), List())
val f3 = new Floor(List(), List())


/*
val f0 = new Floor(List("H", "L"), List())
val f1 = new Floor(List(), List("H"))
val f2 = new Floor(List(), List("L"))
val f3 = new Floor(List(), List())

*/
val initFloors = f0 :: f1 :: f2 ::f3 :: Nil

val initState = new State(initFloors, 0, 0)

initState.isStateSafe
initState.isFinalState


var stateQueue = new mutable.Queue[State]()
stateQueue.enqueue(initState)

var stateHistory = ArrayBuffer[String]()
stateHistory += initState.toString


println("length: " + stateQueue.length)

while (stateQueue.nonEmpty) {
  println("length: " + stateQueue.length)
  for(i <- 1 to stateQueue.length) {
    val s = stateQueue.dequeue()
    if(s.isFinalState) {
        println("----------------------------------------------------------")
        println("steps: " + s.Steps)
        println("----------------------------------------------------------")
    } else {
      val states = s.nextStates
      states.foreach(s => {
        if(!stateHistory.contains(s.toString)){
          //println(s.toString)
          stateHistory += s.toString
          stateQueue.enqueue(s)
        }
      })
    }
  }
}

*/


def calcule(floors: List[Int]): Int = {
  2 * (floors(0) -1 ) - 1 +
    2 * (floors(0) + floors(1)-1 ) - 1 +
    2 * (floors(0) + floors(1) + floors(2)-1 ) - 1
}

calcule(List(4,5,1,0))

calcule(List(8,5,1,0))