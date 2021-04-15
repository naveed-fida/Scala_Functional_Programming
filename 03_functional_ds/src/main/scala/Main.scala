package fpinscala.datastructures

object Main {
  def main(args: Array[String]): Unit = {
    val ints = List(1, 2, 3, 4, 5)
    println("Sum2: " + List.sum2(ints))
    println("tail: " + List.tail(ints))
    println("drop 2: " + List.drop(ints, 2))
    println("initial: " + List.initial(ints))
    println("initial2: " + List.initial2(ints))
    println("length: " + List.length(ints))
    println("foldLeft: " + List.foldLeft(ints, 1)((x, y) => x * y))
    println("Empty List", List[Int]())
  }
}
