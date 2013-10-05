package com.johankahrstrom.expressions

abstract class Expression {
  def evaluate(x: Double): Double
  def derive: Expression
}

case object One extends Expression {
  def evaluate(x: Double) = 1.0
  def derive = Zero
  
  override def toString = "1"
}

case object Zero extends Expression {
  def evaluate(x: Double) = 0.0
  def derive = Zero
  
  override def toString = "0"
}

case class Constant(c: Double) extends Expression {
  def evaluate(x: Double) = c
  def derive = Zero
  
  override def toString = c.toString
}

case object Variable extends Expression {
  def evaluate(x: Double) = x
  def derive = One
  
  override def toString = "x"
}

case class Add(left: Expression, right: Expression) extends Expression {
  def evaluate(x: Double) = left.evaluate(x) + right.evaluate(x)
  def derive = new Add(left.derive, right.derive)
  
  override def toString = "(" + left + " + " + right + ")"
}

case class Negate(expr: Expression) extends Expression{
  def evaluate(x: Double) = -expr.evaluate(x)
  def derive = new Negate(expr.derive)
  
  override def toString = "-(" + expr + ")"
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  def evaluate(x: Double) = left.evaluate(x) * right.evaluate(x)
  def derive = new Add(new Multiply(left.derive, right), new Multiply(left, right.derive))
  
  override def toString = "(" + left + " * " + right + ")"
}

case class Divide(numerator: Expression, denominator: Expression) extends Expression {
  def evaluate(x: Double) = numerator.evaluate(x) / denominator.evaluate(x)
  def derive = new Divide(new Add(new Multiply(numerator.derive, denominator), new Negate(new Multiply(numerator, denominator.derive))), new Multiply(denominator, denominator))
  
  override def toString = numerator.toString + "/" + denominator.toString
}

case class Sine(param: Expression) extends Expression {
  def evaluate(x: Double) = Math.sin(x)
  def derive = new Multiply(new Cosine(param), param.derive)
  
  override def toString = "sin(" + param + ")"
}

case class Cosine(param: Expression) extends Expression {
  def evaluate(x: Double) = Math.cos(x)
  def derive = new Multiply(new Negate(new Sine(param)), param.derive)
  
  override def toString = "cos(" + param + ")"
}