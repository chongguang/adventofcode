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

class State(x: Int, y:Int, history: List[(Int, Int)]){
  val X: Int = x
  val Y: Int = y
  val History = history
}


def apply(s: State):Unit = {
  if (s.X == 31 && s.Y == 39) {
    if(s.History.length < 84)
    println(s.History.length)
  } else {
    if(canMoveUp(s)) apply(new State(s.X, s.Y-1, s.History:+(s.X, s.Y)))
    if(canMoveDown(s)) apply(new State(s.X, s.Y+1, s.History:+(s.X, s.Y)))
    if(canMoveLeft(s)) apply(new State(s.X-1, s.Y, s.History:+(s.X, s.Y)))
    if(canMoveRight(s)) apply(new State(s.X+1, s.Y, s.History:+(s.X, s.Y)))
  }
}

apply(new State(1,1,List[(Int, Int)]()))