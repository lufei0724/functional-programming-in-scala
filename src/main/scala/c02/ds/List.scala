package c02.ds

import c02.ds.List.{add1, addOneViaFoldLeft, drop, dropWhile, init, length, sum, sum2, sum3, sumList, tail, transDoubleToString}

/**
 * Introduce a data type with the trait keyword
 * sealed means that all implementations of List must be declared in this file
 * type parameter [+A] to declare the List data type to be polymorphic
 * the + indicates the type parameter A is a covariant parameter
 * Making A covariant implies that for two types A and B where B is a subtype of A, then List[B] is a subtype of List[A]
 */
sealed trait List[+A]

/**
 * Following case to represent the two possible forms a list can take
 */
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * List companion object
 * Contains functions for creating and working with lists
 *
 */
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](s: List[A]): List[A] = s match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], s: A): List[A] = l match {
    case Nil => List(s)
    case Cons(head, tail) => Cons(s, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(head, tail) => drop(tail, n - 1)
    }
  }

  /**
   * When a function definition contains multiple argument groups
   * type information flows from left to right across these argument groups
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail)(f) else Cons(head, dropWhile(tail)(f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, Cons(b, tail)) => if (tail == Nil) Cons(a, Nil) else Cons(a, init(Cons(b, tail)))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)

    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) =
    foldRight(ns, 0)(_ * _)

  def product3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, x) => 1 + x)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((a, b) => Cons(b, a))

  def appendViaFoldRight[A, B](as: List[A], z: List[A]): List[A] =
    foldRight(as, z)(Cons(_, _))

  def append[A](r: List[A], l: List[A]): List[A] = r match {
    case Nil => l
    case Cons(head, tail) => Cons(head, append(tail, l))
  }

  def concate[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def addOneViaFoldLeft(as: List[Int]): List[Int] =
    reverse(foldLeft(as, Nil: List[Int])((a, b) => Cons(b + 1, a)))

  def transDoubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concate(map(as)(f))

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def sumList(r: List[Int], l: List[Int]): List[Int] = (r, l) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, b), Cons(c, d)) => Cons(a + c, sumList(b, d))
  }

  def zipWith[A](r: List[A], l: List[A])(f: (A, A) => A): List[A] = (r, l) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, b), Cons(c, d)) => Cons(f(a, c), zipWith(b, d)(f))
  }

  @annotation.tailrec
  def startWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)   => true
    case (Cons(x, y), Cons(x1, y1)) if x == x1 => startWith(y, y1)
    case _ => false
  }

  @annotation.tailrec
  def hasSubSeq[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startWith(sup, sub) => true
    case Cons(h, t) => hasSubSeq(t, sub)
  }


  /**
   * Variadic function meaning it accepts zero or more arguments of type A
   * It is providing syntactic sugar for creating and passing a Seq of elements explicitly
   * so we can invoke it with syntax like List(1, 2, 3, 4)
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

