package c02.ds
import List._
import Tree._

object Main extends App {

  val ex1: List[Double] = Nil
  val list = List(1, 2, 3, 4)
  println(drop(list, 2))
  println(dropWhile(list)(a => a < 3))
  println(dropWhile(list)(a => a > 2))
  println(init(list))
  println(length(list))
  println(sum2(list))
  println(sum3(list))
  println(add1(list))
  println(addOneViaFoldLeft(list))

  val dList = List(1.2, 2.3, 3, 2)
  println(transDoubleToString(dList))

  println("===map List(1, 2, 3) with i => i + 1")
  val mapList = List(1, 2, 3)
  println(List.map(mapList)(i => i + 1))

  println("====flatMap List(1, 2, 3) with i => List(i, i)")
  val ex2 = List(1, 2, 3)
  println(List.flatMap(ex2)(i => List(i, i)))

  println("====Append List(1, 2) and List(2, 3)=====")
  val exAppend1 = List(1, 2)
  val exAppend2 = List(2, 3)
  println(List.append(exAppend1, exAppend2))

  println("====Concat List(List(1, 2), List(3, 4)) =====")
  val exConcat = List(List(1, 2), List(3, 4))
  println(List.concate(exConcat))

  val evenList = List.filter(ex2)(_ % 2 != 0)
  val evenList2 = List.filterByFlatMap(ex2)(_ % 2 != 0)
  println(evenList)
  println(evenList2)

  println("====Sum List(1,2,3) List(4,5,6)====")
  val sumList1 = List(1, 2, 3)
  val sumList2 = List(4, 5, 6)
  println(List.sumList(sumList1, sumList2))

  println("====zip List(1,2,3) List(4,5,6) with x + y====")
  val zipList1 = List(1, 2, 3)
  val zipList2 = List(4, 5, 6)
  println(List.zipWith(zipList1, zipList2)(_ + _))

  println("====size tree====")
  val t = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  println(Tree.size(t))

  println("====size tree via sizeViaFold====")
  println(Tree.sizeViaFold(t))

  println("====maximum int in tree====")
  val maxIntTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  println(Tree.maximum(maxIntTree))

  println("====maximum int of tree via fold====")
  println(Tree.maxViaFold(maxIntTree))

  println("====depth of tree ====")
  val depthTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
  println(Tree.depth(depthTree))

  println("====depth of tree via fold====")
  println(Tree.depthViaFold(depthTree))

  println("=====map tree =====")
  val mapTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
  println(Tree.map(mapTree)(_ + 1))

}
