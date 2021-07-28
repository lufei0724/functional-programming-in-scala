import c04.strictness._
val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList
ones.forAll(_ != 1)
ones.takeWhile(_ == 1)