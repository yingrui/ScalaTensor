package me.yingrui.tensor

import org.scalatest.{FunSuite, Matchers}
import me.yingrui.tensor.TensorType._

class TensorInitializationTest extends FunSuite with Matchers {

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

}
