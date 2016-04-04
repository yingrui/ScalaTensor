package me.yingrui.tensor

trait TensorType {
  val name: String
  def + (right: TensorType): TensorType = ADD(this, right)
}

case class Variable(name: String) extends TensorType

case class Real(value: Double) extends TensorType {
  val name = value.toString
}

case class ADD(left: TensorType, right: TensorType) extends TensorType {
  val name = s"(${left.name} + ${right.name})"
  def function(x: Map[String, Double]): Double = {
    getResult(x, left) + getResult(x, right)
  }

  def getResult(x: Map[String, Double], expr: TensorType): Double = {
    val f = TensorType.function(Array(expr), expr)
    expr match {
      case Variable(name) => f(Map(name -> x(name)))
      case Real(value) => value
      case _ => f(x)
    }
  }
}

object TensorType {

  def function(parameters: Array[TensorType], tensor: TensorType): Map[String, Double] => Double = {
    tensor match {
      case variable: Variable => (x: Map[String, Double]) => x(variable.name)
      case r: Real => (x: Map[String, Double]) => r.value
      case add: ADD => (x: Map[String, Double]) => add.function(x)
    }
  }
}