package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import List._

@RunWith(classOf[JUnitRunner])
class ListTests extends FunSuite {

  test("tail should work") {
    assert(List.tail(List(1, 2, 3)) === List(2, 3))
  }

  test("setHead prepends element") {
    assert(List.setHead(List(1, 2, 3), 6) == List(6, 1, 2, 3))
  }

  test("drop removes n elements") {
    assert(List.drop(List(1, 2, 3, 4), 2) === List(3, 4))
  }

  test("drop while it's hot") {
    val hot: Int = 5
    val cold: Int = 7
    assert(List.dropWhile(List(hot, hot, hot, cold, cold, hot), (a: Int) => a == hot) == List(cold, cold, hot))
  }

  test("init returns everything but last element") {
    assert(List.init(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4))
  }

  test("length counts number of elements") {
    assert(List.length(List()) == 0)
    assert(List.length(List(1, 3, 4, 5)) == 4)
  }

  test("foldRight works") {
    assert(List.foldRight(List(1, 2, 3), 0)(_ + _) === 6)
    assert(List.foldRight(List(1, 2, 3), 0)(_ - _) === 2)
  }

  test("foldLeft works") {
    assert(List.foldLeft(List(1, 2, 3), 0)(_ + _) === 6)
    assert(List.foldLeft(List(1, 2, 3), 0)(_ - _) === -6)
  }

  test("map works") {
    assert(List.map(List(1, 2, 3, 4))( x => x * x) == List(1, 4, 9, 16))
  }

  test("reverse works") {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("flatten works") {
    assert(flatten(List(List(1, 2), List(3, 4))) == List(1,2,3,4))
  }

  test("filter works") {
    assert(filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))
  }

  test("flatmap works") {
    assert(flatMap(List(1, 2, 3))(x => List(x, x, x)) == List(1, 1, 1, 2, 2, 2, 3, 3, 3))
  }

  test("zipWith works") {
    assert(zipWith(List(1,2,3), List(4,5,6))(_ + _) == List(5, 7, 9))
  }

}
