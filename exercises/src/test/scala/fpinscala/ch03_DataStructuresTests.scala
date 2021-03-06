package fpinscala

import fpinscala.datastructures._
import org.scalatest.FunSuite

/**
  * Created by hans on 19/10/14.
  */
class ch03_DataStructuresTests extends FunSuite {

  import fpinscala.datastructures.List._

  test("ex 3.1") {
    assertResult(3) {
      x
    }
  }
  test("tail") {
    intercept[RuntimeException] {
      tail(Nil)
    }
    assertResult(List(2, 3, 4)) {
      tail(List(1, 2, 3, 4))
    }
  }

  test("setHead") {
    intercept[RuntimeException] {
      setHead(Nil, 2)
    }
    assertResult(List(5, 2, 3, 4)) {
      setHead(List(1, 2, 3, 4), 5)
    }
    assertResult(List("Hi")) {
      setHead(List("Hello"), "Hi")
    }
  }

  test("drop") {
    intercept[RuntimeException] {
      drop(Nil, 1)
    }
    assertResult(List(2, 3, 4), "basic") {
      drop(List(1, 2, 3, 4), 1)
    }
    assertResult(List(1, 2, 3, 4), "n=0") {
      drop(List(1, 2, 3, 4), 0)
    }
    assertResult(List(1, 2, 3, 4), "neg n") {
      drop(List(1, 2, 3, 4), -1)
    }
    assertResult(Nil) {
      drop(List(1, 2, 3, 4), 4)
    }
  }

  test("tail2") {
    intercept[RuntimeException] {
      tail2(Nil)
    }
    assertResult(List(2, 3, 4)) {
      tail2(List(1, 2, 3, 4))
    }
  }

  test("dropWhile") {
    assertResult(Nil) {
      dropWhile(Nil, (x: Int) => false)
    }
    assertResult(List(2, 3, 4), "cond met") {
      dropWhile(List(1, 2, 3, 4), (x: Int) => x < 2)
    }
    assertResult(List(1, 2, 3, 4), "cond not met") {
      dropWhile(List(1, 2, 3, 4), (x: Int) => x > 2)
    }
    assertResult(List("Bonjour"), "strings") {
      dropWhile(List("Hello", "Hi", "Bonjour"), (x: String) => x.startsWith("H"))
    }
  }

  test("init") {
    intercept[RuntimeException] {
      init(Nil)
    }
    assertResult(Nil, "1 element") {
      init(List(1))
    }
    assertResult(List(1, 2, 3), "default") {
      init(List(1, 2, 3, 4))
    }
  }

  test("init2") {
    intercept[RuntimeException] {
      init2(Nil)
    }
    assertResult(Nil, "1 element") {
      init2(List(1))
    }
    assertResult(List(1, 2, 3), "default") {
      init2(List(1, 2, 3, 4))
    }
  }

  test("foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))") {
    val res = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(res)
  }

  test("length") {
    assertResult(0)(length(Nil))
    assertResult(1)(length(List(1)))
    assertResult(3)(length(List(1, 2, 4)))
  }

  test("sum and product - using foldLeft") {
    assertResult(0) {
      sumLF(Nil)
    }
    assertResult(10) {
      sumLF(List(1, 2, 3, 4))
    }
    assertResult(1) {
      productLF(Nil)
    }
    assertResult(24) {
      productLF(List(1, 2, 3, 4))
    }
  }

  test("reverse") {
    assertResult(Nil)(reverse(Nil))
    assertResult(List(1))(reverse(List(1)))
    assertResult(List(1, 2, 3))(reverse(List(3, 2, 1)))
  }

  test("foldRightViaFoldLeft") {
    val res = foldRightViaFoldLeft(List(1, 2, 3, 4), 0)(_ + _)
    assertResult(10)(res)
    val res2 = foldRightViaFoldLeft(List[Int](), 0)(_ + _)
    assertResult(0)(res2)
  }

  test("append RF") {
    assertResult(List[Int]())(appendRF(List[Int](), List[Int]()))
    assertResult(List(1, 2), "appended nil")(appendRF(List(1, 2), List[Int]()))
    assertResult(List(1, 2), "appendee nil")(appendRF(List(), List(1, 2)))
    assertResult(List(1, 2, 3), "both not nil")(appendRF(List(1, 2), List(3)))
  }

  test("append LF") {
    assertResult(List[Int]())(appendLF(List[Int](), List[Int]()))
    assertResult(List(1, 2), "appended nil")(appendLF(List(1, 2), List[Int]()))
    assertResult(List(1, 2), "appendee nil")(appendLF(List(), List(1, 2)))
    assertResult(List(1, 2, 3), "both not nil")(appendLF(List(1, 2), List(3)))
  }

  test("concat") {
    assertResult(List[Int]())(concat(List(List[Int]())))
    assertResult(List[Int](1, 2))(concat(List(List[Int](1, 2))))
    assertResult(List[Int](1, 2, 3, 4, 5))(concat(List(List[Int](1, 2), List[Int](3, 4, 5))))
  }

  test("add1") {
    assertResult(List[Int]())(add1(List[Int]()))
    assertResult(List[Int](2, 3, 4))(add1(List(1, 2, 3)))
  }

  test("doubleToString") {
    assertResult(List[String]())(doubleToString(List[Double]()))
    assertResult(List[String]("1.0", "2.2", "3.33"))(doubleToString(List(1.0, 2.2, 3.33)))
  }

  test("map") {
    assertResult(List(2, 4, 8)) {
      map(List(1, 2, 4))(_ * 2)
    }
  }

  test("map_tr") {
    assertResult(List(2, 4, 8)) {
      map_tr(List(1, 2, 4))(_ * 2)
    }
  }

  test("filter") {
    assertResult(List(2, 4, 8)) {
      filter(List(1, 2, 4, 8, 5))(_ % 2 == 0)
    }
  }

  test("flatMap") {
    assertResult(List(1, 1, 2, 2, 3, 3)) {
      flatMap(List(1, 2, 3))(i => List(i, i))
    }
  }

  test("flatMap_2") {
    assertResult(List(1, 1, 2, 2, 3, 3)) {
      flatMap_2(List(1, 2, 3))(i => List(i, i))
    }
  }

  test("filterWithFlatMap") {
    assertResult(List(2, 4, 8)) {
      filterWithFlatMap(List(1, 2, 4, 8, 5))(_ % 2 == 0)
    }
  }

  test("zipAdd") {
    assertResult(List(5, 7, 9)) {
      zipAdd(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("zipAdd_2") {
    assertResult(List(5, 7, 9)) {
      zipAdd_2(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("zipAdd_3") {
    assertResult(List(5, 7, 9)) {
      zipAdd_3(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("zipWith") {
    assertResult(List(5, 7, 9)) {
      zipWith(List(1, 2, 3), List(4, 5, 6))((x, y) => x + y)
    }
  }

  test("hasSubSequence") {
    assertResult(true, "1,2")(hasSubSequence(List(1, 2, 3, 4), List(1, 2)))
    assertResult(true, "2,3")(hasSubSequence(List(1, 2, 3, 4), List(2, 3)))
    assertResult(true, "4")(hasSubSequence(List(1, 2, 3, 4), List(4)))
    assertResult(false, "1,3")(hasSubSequence(List(1, 2, 3, 4), List(1, 3)))
    assertResult(false, "nil sup")(hasSubSequence(Nil, List(1, 3)))
    assertResult(true, "nil sub")(hasSubSequence(List(1, 2, 3, 4), Nil))
    assertResult(true, "1 1 2 3 4, 1 2 3")(hasSubSequence(List(1, 1, 2, 3, 4), List(1, 2, 3)))
  }

  test("tree - size") {
    import fpinscala.datastructures.Tree._
    assertResult(1, "1 leaf")(size(Leaf("abc")))
    assertResult(5, "3 leafs + 2 branches")(
      size(Branch(Leaf("abc"), Branch(Leaf("def"), Leaf("ghi")))))
  }

  test("tree - maximum") {
    import fpinscala.datastructures.Tree._
    assertResult(1, "1 leaf")(maximum(Leaf(1)))
    assertResult(3, "3 leafs + 2 branches")(maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))))
  }

  test("tree - depth") {
    import fpinscala.datastructures.Tree._
    assertResult(0, "1 leaf")(depth(Leaf(1)))
    assertResult(2, "3 leafs + 2 branches")(depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))))
    assertResult(3, "3 leafs + 3 branches")(
      depth(Branch(Leaf(1), Branch(Branch(Leaf(3), Leaf(8)), Leaf(2)))))
  }

  test("tree - map") {
    import fpinscala.datastructures.Tree._
    assertResult(Leaf(2), "1 leaf")(map(Leaf(1))((x: Int) => x + 1))
    assertResult(Branch(Leaf("1"), Branch(Branch(Leaf("3"), Leaf("8")), Leaf("2"))),
                 "3 leafs + 3 branches") {
      map(Branch(Leaf(1), Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))))((x: Int) => x.toString)
    }
  }

  test("tree - fold - size") {
    import fpinscala.datastructures.Tree._
    assertResult(1, "1 leaf")(sizeViaFold(Leaf("abc")))
    assertResult(5, "3 leafs + 2 branches")(
      sizeViaFold(Branch(Leaf("abc"), Branch(Leaf("def"), Leaf("ghi")))))
  }

  test("tree - fold - maximum") {
    import fpinscala.datastructures.Tree._
    assertResult(1, "1 leaf")(maximumViaFold(Leaf(1)))
    assertResult(3, "3 leafs + 2 branches")(
      maximumViaFold(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))))
  }

  test("tree - fold - map") {
    import fpinscala.datastructures.Tree._
    assertResult(Leaf(2), "1 leaf")(mapViaFold(Leaf(1))((x: Int) => x + 1))
    assertResult(Branch(Leaf("1"), Branch(Branch(Leaf("3"), Leaf("8")), Leaf("2"))),
                 "3 leafs + 3 branches") {
      mapViaFold(Branch(Leaf(1), Branch(Branch(Leaf(3), Leaf(8)), Leaf(2))))((x: Int) =>
        x.toString)
    }
  }

  test("tree - fold - depth") {
    import fpinscala.datastructures.Tree._
    assertResult(0, "1 leaf")(depth(Leaf(1)))
    assertResult(2, "3 leafs + 2 branches")(
      depthViaFold(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))))
    assertResult(3, "3 leafs + 3 branches")(
      depthViaFold(Branch(Leaf(1), Branch(Branch(Leaf(3), Leaf(8)), Leaf(2)))))
  }

}
