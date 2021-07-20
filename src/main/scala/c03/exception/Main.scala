package c03.exception

import Option._
import Either._

object Main extends App {
  println("==== Some(1).map(_ => 2) ====")
  val s1 = Some(1)
  println(s1.map(_ => 2))

  println("==== None.map(_ => 2) ====")
  val s2 = None
  println(s2.map(_ => 2))

  println("==== Some(1).flatMap(Some(_ + 1))")
  val sFlatMap = Some(1)
  println(sFlatMap.flatMap(a => Some(a + 1)))

  println("==== Some(1).flatMapViaMap(Some(_ + 1))")
  println(sFlatMap.flatMapViaMap(a => Some(a + 1)))

  println("==== None.flatMap(Some(2))")
  val n1 = None
  println(n1.flatMap(_ => Some(2)))

  println("==== Some(2).getOrElse(4) ====")
  val s3 = Some(2)
  println(s3.getOrElse(4))

  println("==== None.getOrElse(4) ====")
  val s4 = None
  println(s4.getOrElse(4))

  println("==== None.getOrElse(None) ====")
  println(s4.getOrElse(None))

  println("==== Some(2).orElse(Some(4)) ====")
  val s5 = Some(2)
  println(s5.orElse(Some(4)))

  println("==== Some(2).orElseViaMap(Some(4)) ====")
  println(s5.orElseViaMap(Some(4)))

  println("==== None.orElse(Some(4)) ====")
  val s6 = None
  println(s6.orElse(Some(4)))

  println("==== Some(2).filter(_ == 2) ====")
  val s7 = Some(2)
  println(s7.filter(_ == 2))

  println("==== Some(2).filter(_ == 3) ====")
  val s8 = Some(2)
  println(s8.filter(_ == 3))

  println("==== None.filter(_ == 3) ====")
  val s9 = None
  println(s9.filter(_ == 3))

  println("==== sequence List(Some(1), Some(2), Some(3)) ==== ")
  val s10 = List(Some(1), Some(2), Some(3))
  println(sequence(s10))

  println("==== sequence List(Some(1), None, Some(3)) ==== ")
  val s11 = List(Some(1), None, Some(3))
  println(sequence(s11))

  println("==== traverse List(1, 2, 3) with Some(_ + 1) ==== ")
  val s12 = List(1, 2, 3)
  println(traverse(s12)(a => Some(a + 1)))

  println("==== traverse List(1, \"abc\", 3) with Some(_ + 1) ==== ")
  val s13 = List("1", "abc", "3")
  //println(traverse(s13)(a => Some(a.toInt + 1)))

  println("=== parseInsuranceRateQuote('30', '2')")
  println(Quote.parseInsuranceRateQuote("30", "2"))

  println("=== InsuranceRateQuote('30', 'abc')")
  println(Quote.parseInsuranceRateQuote("30", "abc"))
}
