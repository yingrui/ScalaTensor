package me.yingrui.tensor

import org.scalatest.{FunSuite, Matchers}

class TensorMacroTest extends FunSuite with Matchers {

  test("should printf") {
    import TensorMacro._
    printf1("%d is %s", 10, "int")
  }

}
