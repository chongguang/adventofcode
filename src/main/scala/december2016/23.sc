import scala.io.Source


case class State(Registers: Map[String, Int], Instructions: List[(Transformation, String)], InstructionPointer: Int)

def isFinished(s:State): Boolean = s.InstructionPointer >= s.Instructions.length

type Transformation = State => State

def Parser(str: String): Transformation = {
  val list = str.split(" ")
  list(0) match {
    case "inc" => inc(list(1))
    case "dec" => dec(list(1))
    case "jnz" => jnz(list(1), list(2))
    case "tgl" => tgl(list(1))
    case _ => cpy(list(1), list(2))
  }
}


def tgl(key:String): Transformation = (s:State) => {
  val index = s.Registers(key) + s.InstructionPointer
  if(index >= s.Instructions.length || index < 0) State(s.Registers, s.Instructions, s.InstructionPointer+1)
  else {
    val oldInstructionString = s.Instructions(index)._2
    val oldList = oldInstructionString.split(" ").toList
    val newList = oldList.length match {
      case 2 => oldList.head match {
        case "inc" => "dec" :: oldList.tail
        case _ => "inc" :: oldList.tail
      }
      case _ => oldList.head match {
        case "jnz" => "cpy" :: oldList.tail
        case _ => "jnz" :: oldList.tail
      }
    }
    val isValide = isInstructionValide(newList)
    val newString = newList.mkString(" ")
    val newIns: Transformation = if(isValide) Parser(newString) else (s: State) => s
    val newPair : (Transformation, String)= (newIns, newString)
    val front: List[(Transformation, String)] = s.Instructions.take(index)
    val back: List[(Transformation, String)] = s.Instructions.drop(index+1)
    val newInsList: List[(Transformation, String)] = front ++ (newPair :: back)
    State(s.Registers, newInsList, s.InstructionPointer+1)

  }
}

def isInstructionValide(list:List[String]): Boolean = {
  list.head match {
    case "inc" => if(isAllDigits(list(1))) false else true
    case "dec" => if(isAllDigits(list(1))) false else true
    case "cpy" => if(isAllDigits(list(2))) false else true
    case _ => true
  }
}

def inc(key: String): Transformation = (s: State) => State(s.Registers + (key -> (s.Registers(key) + 1)), s.Instructions, s.InstructionPointer + 1)

def dec(key: String): Transformation = (s: State) => State(s.Registers + (key -> (s.Registers(key) - 1)), s.Instructions, s.InstructionPointer + 1)

def jnz(key: String, jump: String): Transformation = (s: State) => {
  val t = if(isIntLike(key)) key.toInt else s.Registers(key)
  val steps = if(isIntLike(jump)) jump.toInt else s.Registers(jump)

  t match {
    case 0 => State(s.Registers, s.Instructions, s.InstructionPointer + 1)
    case _ => State(s.Registers, s.Instructions, s.InstructionPointer + steps)
  }
}

def isAllDigits(x: String) = x forall Character.isDigit

def isIntLike(x: String) = {
  x.head match {
    case '-' => isAllDigits(x.tail)
    case _ => isAllDigits(x)
  }
}

def cpy(from: String, to: String): Transformation = (s: State) => {
  if (isIntLike(from)) State(s.Registers + ( to -> from.toInt), s.Instructions, s.InstructionPointer + 1)
  else State(s.Registers + ( to -> s.Registers(from)), s.Instructions, s.InstructionPointer + 1)
}

val stream : String = getClass.getResource("/day23input.txt").getPath
val instructionList = Source.fromFile( stream ).getLines.toList.map(s => (Parser(s), s))

def transform(state: State): State = {
  if (isFinished(state)) state
  else transform(state.Instructions(state.InstructionPointer)._1(state))
}

val initMap: Map[String, Int] = Map("a"->12, "b"->0, "c"->0, "d"->0)
val initState: State = State(initMap, instructionList, 0)
val finalState = transform(initState)
finalState.Registers.toString()
