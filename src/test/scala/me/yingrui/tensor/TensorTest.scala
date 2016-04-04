package me.yingrui.tensor

import org.scalatest.{FunSuite, Matchers}
import me.yingrui.tensor.TensorType._

class TensorTest extends FunSuite with Matchers {

  test("should create a variable") {
    val x = Variable("x")
    val f = function(Array(x), x)

    f(Map("x" -> 1)) shouldEqual 1
  }

  test("should create a real") {
    val n = Real(1)
    val f = function(Array(n), n)

    f(Map()) shouldEqual 1
  }

  test("should create add tensor: x + y") {
    val x = Variable("x")
    val y = Variable("y")
    val z = x + y
    z.name shouldEqual "(x + y)"

    val f = function(Array(x, y), z)
    f(Map("x" -> 1, "y" -> 2)) shouldEqual 3

    val m = Variable("m")
    val l = x + y + m

    l.name shouldEqual "((x + y) + m)"

    val g = function(Array(x, y, m), l)
    g(Map("x" -> 1, "y" -> 2, "m" -> 3)) shouldEqual 6
  }

  test("should create add tensor: x + x") {
    val x = Variable("x")
    val y = x + x
    y.name shouldEqual "(x + x)"

    val f = function(Array(x), y)
    f(Map("x" -> 1)) shouldEqual 2
  }

  test("should create add tensor: 10 + x") {
    val x = Variable("x")
    val z = Real(10) + x
    z.name shouldEqual "(10.0 + x)"

    val f = function(Array(x), z)
    f(Map("x" -> 1)) shouldEqual 11
  }
}
