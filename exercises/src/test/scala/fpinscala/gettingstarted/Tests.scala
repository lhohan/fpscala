package fpinscala.gettingstarted

import org.scalatest.FunSuite

/**
 * Created by hans on 17/10/14.
 */
class Tests extends FunSuite{


  test("isSorted"){
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

}
