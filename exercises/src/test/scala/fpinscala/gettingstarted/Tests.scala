package fpinscala.gettingstarted

import org.scalatest.FunSuite

/**
 * Created by hans on 17/10/14.
 */
class Tests extends FunSuite{


  test("isSorted - Int"){
    import fpinscala.gettingstarted.PolymorphicFunctions.isSorted
    def gt1 = (x:Int, y:Int) => x <= y
    assertResult(true, "empty") {isSorted(Array(),gt1)}
    assertResult(true, "one") {isSorted(Array(5),gt1)}
    assertResult(true, "sorted 2") {isSorted(Array(3,6),gt1)}
    assertResult(true, "sorted 2 - same") {isSorted(Array(3,3),gt1)}
    assertResult(true, "sorted - 1") {isSorted(Array(1,3,5,6,6),gt1)}
    assertResult(true, "sorted - 2") {isSorted(Array(-1,3,5,6,6),gt1)}
    assertResult(false, "unsorted - 1") {isSorted(Array(-1,7,5),gt1)}
    assertResult(false, "unsorted - 2") {isSorted(Array(4,3),gt1)}
  }

  test("isSorted - String"){
    import fpinscala.gettingstarted.PolymorphicFunctions.isSorted
    def gt1(x:String, y:String) = x.length <= y.length
    assertResult(true, "empty") {isSorted(Array(),gt1)}
    assertResult(true, "one") {isSorted(Array("abc"),gt1)}
    assertResult(true, "sorted 2") {isSorted(Array("abc","abcdef"),gt1)}
    assertResult(true, "sorted 2 - same") {isSorted(Array("ab","ab"),gt1)}
    assertResult(true, "sorted - 1") {isSorted(Array("a","ab","xy","qwerty","qwertyuiop"),gt1)}
    assertResult(false, "unsorted - 1") {isSorted(Array("abcd","abcdef","a"),gt1)}
  }

  test("curry"){
    import fpinscala.gettingstarted.PolymorphicFunctions.curry
    curry((x:Int, y:Double) => (x+y).toString)

  }

  test("compose"){
    import fpinscala.gettingstarted.PolymorphicFunctions.compose

    // compose to functions:
    // - one takes and array I determines it is empty or not giving back a boolean
    // - the other takes that boolean and turns it into a char 'F' or 'T'

    def isEmpty[T] (a: Array[T]) = a.isEmpty
    def boolToChar(b:Boolean):Char =  if(b) 'T' else 'F'
    def h = compose(boolToChar , isEmpty[String])
    assertResult('T'){h(Array())}
    assertResult('F'){h(Array("not empty"))}
  }

}
