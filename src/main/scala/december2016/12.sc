import scala.io.Source

val stream : String = getClass.getResource("/day12input.txt").getPath
val instructionList = Source.fromFile( stream ).getLines.toList.map(s => Parser(s))


val initMap: Map[String, Int] = Map("a"->0, "b"->0, "c"->1, "d"->0)
val initState: State = new State(initMap, instructionList, 0)
val finalState = apply(initState)
finalState.Registers.toString()

def apply(state: State): State = {
  if (state.isFinished) state
  else apply(state.Instructions(state.InstructionPointer)(state))
}


class State(rs: Map[String, Int], ins: List[State => State], ip: Int){
  val Registers: Map[String, Int] = rs
  val Instructions: List[State => State] = ins
  val InstructionPointer: Int = ip

  def isFinished: Boolean = InstructionPointer == Instructions.length

  //def apply: State = if(this.isFinished) this else Instructions(InstructionPointer)(this).apply
}


case class St (rs:Map[String, Int])

def Parser(str: String): State => State = {
  val list = str.split(" ")
  list(0) match {
    case "inc" => inc(list(1))
    case "dec" => dec(list(1))
    case "jnz" => jnz(list(1), list(2))
    case _ => cpy(list(1), list(2))
  }
}

def inc(key: String): State => State = (s: State) => new State(s.Registers + (key -> (s.Registers(key) + 1)), s.Instructions, s.InstructionPointer + 1)

def dec(key: String): State => State = (s: State) => new State(s.Registers + (key -> (s.Registers(key) - 1)), s.Instructions, s.InstructionPointer + 1)

def jnz(key: String, jump: String): State => State = (s: State) => {

  val t = isAllDigits(key) match {
    case true => key.toInt
    case _ => s.Registers(key)
  }

  t match {
    case 0 => new State(s.Registers, s.Instructions, s.InstructionPointer + 1)
    case _ => new State(s.Registers, s.Instructions, s.InstructionPointer + jump.toInt)
  }
}

def isAllDigits(x: String) = x forall Character.isDigit

def cpy(from: String, to: String): State => State = (s: State) => {
  if (isAllDigits(from)) new State(s.Registers + ( to -> from.toInt), s.Instructions, s.InstructionPointer + 1)
  else new State(s.Registers + ( to -> s.Registers(from)), s.Instructions, s.InstructionPointer + 1)
}

/*
cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 19 c
cpy 11 d
inc a
dec d
jnz d -2
dec c
jnz c -5
 */