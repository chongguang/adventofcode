package december2016
import scala.io.Source

/**
  * Created by Chongguang on 2016/12/25.
  */
object Day25 {

  type Transformation = State => State

  case class State(Registers: Map[String, Int], Instructions: List[Transformation], InstructionPointer: Int, Output: List[Int])

  def isFinished(s:State): Boolean = s.InstructionPointer >= s.Instructions.length

  def isGoodSignal(s:State): Boolean = s.Output.zipWithIndex.count(i => {
    val rest = i._2 % 2
    if (rest == 0) i._1 == 0 else i._1 == 1
  }) == s.Output.length

  def transform(state: State): State = {
    if (isFinished(state)) state
    else transform(state.Instructions(state.InstructionPointer)(state))
  }

  def Parser(str: String): Transformation = {
    val list = str.split(" ")
    list(0) match {
      case "inc" => inc(list(1))
      case "dec" => dec(list(1))
      case "jnz" => jnz(list(1), list(2))
      case "out" => out(list(1))
      case _ => cpy(list(1), list(2))
    }
  }

  def out(key: String): Transformation = (s:State) => {
    //println(s.Registers(key))
    //println("a is: " + s.Registers("a"))
    val newOutput = s.Output :+ s.Registers(key)
    val newPointer = if(newOutput.length >= 12) Int.MaxValue else s.InstructionPointer+1
    State(s.Registers, s.Instructions, newPointer, newOutput)
  }

  def inc(key: String): Transformation = (s: State) => State(s.Registers + (key -> (s.Registers(key) + 1)), s.Instructions, s.InstructionPointer + 1, s.Output)

  def dec(key: String): Transformation = (s: State) => State(s.Registers + (key -> (s.Registers(key) - 1)), s.Instructions, s.InstructionPointer + 1, s.Output)

  def jnz(key: String, jump: String): Transformation = (s: State) => {
    val t = if(isIntLike(key)) key.toInt else s.Registers(key)
    val steps = if(isIntLike(jump)) jump.toInt else s.Registers(jump)

    t match {
      case 0 => State(s.Registers, s.Instructions, s.InstructionPointer + 1, s.Output)
      case _ => State(s.Registers, s.Instructions, s.InstructionPointer + steps, s.Output)
    }
  }

  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def isIntLike(x: String):Boolean = {
    x.head match {
      case '-' => isAllDigits(x.tail)
      case _ => isAllDigits(x)
    }
  }

  def cpy(from: String, to: String): State => State = (s: State) => {
    if (isIntLike(from)) State(s.Registers + ( to -> from.toInt), s.Instructions, s.InstructionPointer + 1, s.Output)
    else State(s.Registers + ( to -> s.Registers(from)), s.Instructions, s.InstructionPointer + 1, s.Output)
  }

  val stream : String = getClass.getResource("/day25input.txt").getPath
  val instructionList: List[Transformation] = Source.fromFile( stream ).getLines.toList.map(s => Parser(s))

  def main(args: Array[String]): Unit = {

    //val t = isGoodSignal(State(Map("a"->1, "b"->0, "c"->0, "d"->0), List[Transformation](), 1, List(0,1,1,1)))
    //print(t)


    for(
      i <- 0 to 1

    ) yield {
      val initMap: Map[String, Int] = Map("a"->i, "b"->0, "c"->0, "d"->0);
      val initState: State = State(initMap, instructionList, 0, List[Int]());
      val finalState: State = transform(initState)//;
      println(finalState.Output.reverse.mkString)
      //if (isGoodSignal(finalState))

    }

    /*
    val initMap: Map[String, Int] = Map("a"->2, "b"->0, "c"->0, "d"->0)
    val initState: State = State(initMap, instructionList, 0, List[Int]())
    val finalState: State = transform(initState)
    */
    //println(finalState.Registers.toString())

  }



}
