package fpinscala.datastructures

import org.scalatest.FunSuite

/**
 * Created by hans on 19/10/14.
 */
class Tests extends FunSuite{
  import List._
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
}
