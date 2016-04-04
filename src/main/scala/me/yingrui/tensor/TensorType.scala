package me.yingrui.tensor

trait TensorType {
  val name: String

  def + (right: TensorType): TensorType = Arithmetic(this, right, "+", (x, y) => x + y)
  def - (right: TensorType): TensorType = Arithmetic(this, right, "-", (x, y) => x - y)
  def * (right: TensorType): TensorType = Arithmetic(this, right, "*", (x, y) => x * y)
  def / (right: TensorType): TensorType = Arithmetic(this, right, "/", (x, y) => x / y)
}

case class Variable(name: String) extends TensorType

case class Real(value: Double) extends TensorType {
  val name = value.toString
}

case class Arithmetic(left: TensorType, right: TensorType, sign: String, op: (Double, Double) => Double) extends TensorType {
  val name = s"(${left.name} $sign ${right.name})"
  def function(x: Map[String, Double]): Double = {
    op(getResult(x, left), getResult(x, right))
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
      case add: Arithmetic => (x: Map[String, Double]) => add.function(x)
    }
  }
}