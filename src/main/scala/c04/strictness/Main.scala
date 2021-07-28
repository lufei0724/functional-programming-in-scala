package c04.strictness

object Main extends App {
  println("===== Stream(1, 2, 3, 4) toList ====")
  val s = Stream(1, 2, 3, 4)
  println(s.toList)
  println("===== Stream(1, 2, 3, 4) take(1) toList ====")
  println(s.take(1).toList)
  println("===== Stream(1, 2, 3, 4) take(2) toList ====")
  println(s.take(2).toList)
  println("===== Stream(1, 2, 3, 4) drop(1) toList ====")
  println(s.drop(1).toList)
  println("===== Stream(1, 2, 3, 4) drop(0) toList ====")
  println(s.drop(0).toList)
  println("===== Stream(1, 2, 3, 4) drop(5) toList ====")
  println(s.drop(5).toList)
  println("===== Stream(1, 2, 3, 4) takeWhile(_ > 2) toList ====")
  println(s.takeWhile(_ > 2).toList)
  println("===== Stream(1, 2, 3, 4) takeWhile(_ < 1) toList ====")
  println(s.takeWhile(_ < 1).toList)
  println("===== Stream(1, 2, 3, 4) forAll(_ > 0) ====")
  println(s.forAll(_ > 0))
  println("===== Stream(1, 2, 3, 4) forAll(_ > 2) ====")
  println(s.forAll(_ > 2))
  println("==== Stream(1, 2, 3, 4) map(_ + 1) ===== ")
  println(s.map(_ + 1).toList)
  println("==== Stream(1, 2, 3, 4) mapViaFoldRight(_ + 1) ===== ")
  println(s.mapViaFoldRight(_ + 1).toList)
}
