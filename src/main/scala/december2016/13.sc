import scala.collection.mutable

//val favoriteNumber = 10
val favoriteNumber = 1362

def isOpenSpace(x: Int, y:Int): Boolean = {
  val t = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
  val b = t.toBinaryString.toCharArray.toList.map(_.toInt).sum
  b % 2 == 0
}

def canMoveUp(s : State): Boolean = s.Y - 1 >= 0 && isOpenSpace(s.X, s.Y - 1 ) && !s.History.contains((s.X, s.Y - 1))
def canMoveDown(s : State): Boolean = isOpenSpace(s.X, s.Y + 1 ) && !s.History.contains((s.X, s.Y + 1)) && s.Y + 1 <= 100
def canMoveLeft(s: State): Boolean = s.X - 1 >= 0 && isOpenSpace(s.X - 1, s.Y ) && !s.History.contains((s.X -1, s.Y ))
def canMoveRight(s: State): Boolean = isOpenSpace(s.X + 1, s.Y ) && !s.History.contains((s.X+1, s.Y)) && s.X + 1 <= 100


/*
(0 to 9).map(i => {
  if (isOpenSpace(i, 0)) print(".") else print("#")
})
*/

case class State(X: Int, Y:Int, History: List[(Int, Int)])


def apply(s: State):Unit = s match {
  case State(31, 39, history) if history.length <84 => println(s.History.length)
  case State(x, y ,history) if canMoveUp(s) => apply(State(x, y-1, history:+(x,y)))
  case State(x, y ,history) if canMoveDown(s) => apply(State(x, y+1, history:+(x,y)))
  case State(x, y ,history) if canMoveLeft(s) => apply(State(x-1, y, history:+(x,y)))
  case State(x, y ,history) if canMoveRight(s) => apply(State(x+1, y, history:+(x,y)))
  case _ => println("oups")
}



apply(State(1,1,List[(Int, Int)]()))