package c04.strictness

import Stream._

trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, tl) => h() :: tl().toList
    case _ => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, tl) if n > 1 => cons(h(), tl().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def foldRight[B](z: => B)(f: (A,  => B) => B): B =
    this match {
      case Cons(h, tl) => f(h(), tl().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, tl) => cons(f(h()), tl().map(f))
    case _ => empty
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](z: => Stream[B]): Stream[B] = this match {
    case Cons(hd, tl) => cons(hd(), tl().append(z))
    case _ => z
  }

  def appendViaFoldRight[B>:A](z: => Stream[B]): Stream[B] =
    foldRight(z)((a, b) => cons(a, b))

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  val fibo = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
}