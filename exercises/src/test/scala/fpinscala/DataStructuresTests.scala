package fpinscala

import fpinscala.datastructures.{Cons, List, Nil}
import org.scalatest.FunSuite

/**
 * Created by hans on 19/10/14.
 */
class DataStructuresTests extends FunSuite{
  import fpinscala.datastructures.List._
  test("ex 3.1"){
    assertResult(3){x}
  }
  test("tail"){
    intercept[RuntimeException]{tail(Nil)}
    assertResult(List(2,3,4)){tail(List(1,2,3,4))}
  }

  test("setHead"){
    intercept[RuntimeException]{setHead(Nil, 2)}
    assertResult(List(5,2,3,4)){setHead(List(1,2,3,4),5)}
    assertResult(List("Hi")){setHead(List("Hello"),"Hi")}
  }

  test("drop"){
    intercept[RuntimeException]{drop(Nil, 1)}
    assertResult(List(2,3,4), "basic"){drop(List(1,2,3,4),1)}
    assertResult(List(1,2,3,4), "n=0"){drop(List(1,2,3,4),0)}
    assertResult(List(1,2,3,4), "neg n"){drop(List(1,2,3,4),-1)}
    assertResult(Nil){drop(List(1,2,3,4),4)}
  }

  test("tail2"){
    intercept[RuntimeException]{tail2(Nil)}
    assertResult(List(2,3,4)){tail2(List(1,2,3,4))}
  }

  test("dropWhile"){
    assertResult(Nil){dropWhile(Nil, (x:Int) => false)}
    assertResult(List(2,3,4), "cond met"){dropWhile(List(1,2,3,4), (x:Int) => x < 2)}
    assertResult(List(1,2,3,4), "cond not met"){dropWhile(List(1,2,3,4), (x:Int) => x > 2)}
    assertResult(List("Bonjour"), "strings"){dropWhile(List("Hello","Hi","Bonjour"), (x:String) => x.startsWith("H"))}
  }

  test("init"){
    intercept[RuntimeException]{init(Nil)}
    assertResult(Nil, "1 element"){init(List(1))}
    assertResult(List(1,2,3), "default"){init(List(1,2,3,4))}
  }

  test("init2"){
    intercept[RuntimeException]{init2(Nil)}
    assertResult(Nil, "1 element"){init2(List(1))}
    assertResult(List(1,2,3), "default"){init2(List(1,2,3,4))}
  }

  test("foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))"){
    val res = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(res)
  }

  test("length"){
    assertResult(0)(length(Nil))
    assertResult(1)(length(List(1)))
    assertResult(3)(length(List(1,2,4)))
  }
}
