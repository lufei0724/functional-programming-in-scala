package c02.ds

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(r, l) => 1 + size(r) + size(l)
    case _ => 1
  }
  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((r, l) => 1 + r + l)

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(r, l) => maximum(r) max maximum(l)
    case Leaf(a) => a
  }

  def maxViaFold(t: Tree[Int]): Int =
    fold(t)(n => n)((r, l) => r max l)

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(r, l) => 1 + (depth(r) max depth(l))
    case Leaf(_) => 0
  }

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((r, l) => 1 + (r max l))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(r, l) => Branch(map(r)(f), map(l)(f))
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(n => Leaf(f(n)): Tree[B])((r, l) => Branch(r, l))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B):B = t match {
    case Branch(r, l) => g(fold(r)(f)(g), fold(l)(f)(g))
    case Leaf(a) => f(a)
  }

}


