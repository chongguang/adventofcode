import scala.util.Random

def removeElementAtEven(l :List[Int]): List[Int] = l.zipWithIndex.filter(p=>p._2%2==0).map{p=>p._1}


def Question1(l: List[Int]):Int = {
  l.length match {
    case 1 => l.head
    case x if x%2 == 0 => Question1(removeElementAtEven(l))
    case _ => Question1(l.last :: removeElementAtEven(l.dropRight(1)))
  }
}


//val a = Question1( (1 to 3004953).toList )
//println(a.toString)


case class State(list: List[Int], position: Int)

def indiceToRemove(list: List[Int], position: Int): Int = (position + list.length/2) % list.length
def deleteElementByIndice(list: List[Int], indice: Int): List[Int] = list.take(indice) ++ list.drop(indice+1)
def newIndice(oldlist: List[Int], oldIndice: Int): Int = (oldIndice + 1) % oldlist.length


/*
indiceToRemove(List(1,2,3,4,5),0) == 2
deleteElementByIndice(List(1,2,3,4,5),2) == List(1,2,4,5)
newIndice(List(1,2,3,4,5), 0) == 1

indiceToRemove(List(1,2,4,5),1) == 3
deleteElementByIndice(List(1,2,4,5),3) == List(1,2,4)
newIndice(List(1,2,4,5), 1) == 2

indiceToRemove(List(1,2,4),2) == 0
deleteElementByIndice(List(1,2,4),1) == List(1,4)
newIndice(List(1,2,4), 2) == 0

*/



def Question2(list: List[Int], position: Int): Int = {
  println(list.toString() + " at position: " + position)
  list.length match {
    case 1 => list.head
    case _ => {
      val index2remove = indiceToRemove(list, position)
      val newList = deleteElementByIndice(list, index2remove)
      val newPosition = newIndice(list, position)
      Question2(newList, newPosition)
    }
  }
}

//Question2((1 to 6).toList,0)


private def addGaussianNoise(lat: Double, lon: Double, seed: Option[Long] = None): (Double, Double) = {
  val rand = seed match {
    case Some(s) => new Random(s)
    case _ => Random
  }
  val sigma =  0.0006456
  (lat + sigma * rand.nextGaussian(), lon + sigma * rand.nextGaussian())
}

addGaussianNoise(27.766106,-15.574492)