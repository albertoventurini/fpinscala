package fpinscala.gettingstarted

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import PolymorphicFunctions._

@RunWith(classOf[JUnitRunner])
class GettingStartedTests extends FunSuite {

  test("isSorted with no elements returns true") {
    assert(PolymorphicFunctions.isSorted[Int](Array(), (_, _) => true))
  }

  test("isSorted with one element returns true") {
    assert(PolymorphicFunctions.isSorted[Int](Array(1), (_, _) => true))
  }

  test("isSorted with two unsorted elements returns false") {
    assert(!PolymorphicFunctions.isSorted[Int](Array(2, 1), _ > _))
  }

  test("isSorted with two sorted elements returns true") {
    assert(PolymorphicFunctions.isSorted[Int](Array(1, 2), _ > _))
  }

  test("isSorted with three unsorted elements returns false") {
    assert(!PolymorphicFunctions.isSorted[Int](Array(1, 3, 2), _ > _))
  }


  test("curry should work") {
    val f = (a: Int, b: Int) => a*b
    val g = PolymorphicFunctions.curry(f)

    assert(f(8, 3) === g(8)(3))
  }

  test("uncurry removes flavour") {
    val f = (a: Int) => (b: Int) => a*b
    val g = PolymorphicFunctions.uncurry(f)

    assert(g(8, 3) === f(8)(3))
  }

  test("compose composes") {
    //val f = (a: Int): Double => a + 2

    def g(a: String): Int = a.toInt
    def f(a: Int): Double = a + 0.5

    assert(compose(f, g)("2") === 2.5)
  }
}
