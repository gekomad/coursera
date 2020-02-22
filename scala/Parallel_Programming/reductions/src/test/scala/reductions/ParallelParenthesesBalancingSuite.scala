package reductions

import java.util.concurrent._

import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import ParallelParenthesesBalancing._

import scala.annotation.tailrec

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("balance should work for string of length n") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check("))", false)
    check("((", false)
    check("(d(d)d)d", true)
    check(")(()()", false)
    check("(", false)
  }

  test("par balance") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("()'", true)
    check("())'", false)
    check("a()()", true)
    check(" ('()).')", false)
    check("(')(')", true)
    check("(())", true)
    check(")(", false)
  }

  test("par balance2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("())(", false)
  }

  test("par balance3") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check(")(", false)
  }

  test("par balance4") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("'((()))'", true)
  }

  test("traverse1") {
    val x = ")(".toArray
    val o = traverse1(x, 0, x.length, 0, 0)
    println(x.mkString)
    println(o)
    assert(o == (0, -1))
  }

  test("traverse2") {
    val x = "()".toArray
    val o = traverse1(x, 0, x.length, 0, 0)
    println(x.mkString)
    println(o)
    assert(o == (0, 0))
  }

  test("traverse3") {
    val x = "()(".toArray
    val o = traverse1(x, 0, x.length, 0, 0)
    println(x.mkString)
    println(o)
    assert(o == (1, 0))
  }

  test("traverse4") {
    val x = "())".toArray
    val o = traverse1(x, 0, x.length, 0, 0)
    println(x.mkString)
    println(o)
    assert(o == (-1, -1))
  }

  test("traverse5") {
    val x = "())(".toArray
    val o = traverse1(x, 0, x.length, 0, 0)
    println(x.mkString)
    println(o)
    assert(o == (0,-1))
  }


}