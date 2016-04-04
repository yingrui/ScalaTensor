package me.yingrui.tensor

import me.yingrui.tensor.TensorType._
import org.scalatest.{FunSuite, Matchers}

class TensorArithmeticTest extends FunSuite with Matchers {

  test("should create add tensor: x + y") {
    val x = Variable("x")
    val y = Variable("y")
    val z = x + y
    z.name shouldEqual "(x + y)"

    val f = function(Array(x, y), z)
    f(Map("x" -> 1, "y" -> 2)) shouldEqual 3
  }

  test("should create add tensor: x + y + z") {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val l = x + y + z

    l.name shouldEqual "((x + y) + z)"

    val f = function(Array(x, y, z), l)
    f(Map("x" -> 1, "y" -> 2, "z" -> 3)) shouldEqual 6
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

  test("should create add tensor: x * y") {
    val x = Variable("x")
    val y = Variable("y")
    val z = x * y
    z.name shouldEqual "(x * y)"

    val f = function(Array(x, y), z)
    f(Map("x" -> 2, "y" -> 4)) shouldEqual 8
  }

  test("should create add tensor: x * y * z") {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val l = x * y * z

    l.name shouldEqual "((x * y) * z)"

    val f = function(Array(x, y, z), l)
    f(Map("x" -> 2, "y" -> 3, "z" -> 4)) shouldEqual 24
  }

  test("should create add tensor: x * x") {
    val x = Variable("x")
    val y = x * x
    y.name shouldEqual "(x * x)"

    val f = function(Array(x), y)
    f(Map("x" -> 2)) shouldEqual 4
  }

  test("should create add tensor: 10 * x") {
    val x = Variable("x")
    val z = Real(10) * x
    z.name shouldEqual "(10.0 * x)"

    val f = function(Array(x), z)
    f(Map("x" -> 1)) shouldEqual 10
  }

  test("should create add tensor: x + y * z") {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val l = x / Real(2) + (y - Real(1)) * z

    l.name shouldEqual "((x / 2.0) + ((y - 1.0) * z))"

    val f = function(Array(x, y, z), l)
    f(Map("x" -> 2, "y" -> 3, "z" -> 4)) shouldEqual 2 / 2 + (3 - 1) * 4
  }

}
