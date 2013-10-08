package com.johankahrstrom.expressions

trait Expression {
  def evaluate(x: Double): Double
  def evaluate(x: Expression): Expression
  def derive: Expression
  def simplify = this
}

object Expression {
  def simplify(exp: Expression): Expression = {
    val simpler = exp.simplify
    if (simpler == exp) exp
    else simplify(simpler)
  }
}

case object Zero extends Expression {
  def evaluate(x: Double) = 0.0
  def evaluate(x: Expression) = Zero
  def derive = Zero

  override def toString = "0"
}

case object One extends Expression {
  def evaluate(x: Double) = 1.0
  def evaluate(x: Expression) = One
  def derive = Zero

  override def toString = "1"
}

case class Constant(c: Double) extends Expression {
  def evaluate(x: Double) = c
  def evaluate(x: Expression) = this
  def derive = Zero

  override def toString = c.toString
}

case object Variable extends Expression {
  def evaluate(x: Double) = x
  def evaluate(x: Expression) = x
  def derive = One

  override def toString = "x"
}

case class Negate(expr: Expression) extends Expression {
  def evaluate(x: Double) = -expr.evaluate(x)
  def evaluate(x: Expression) = Negate(expr.evaluate(x))
  def derive = new Negate(expr.derive)

  override def simplify = expr match {
    case Negate(innerExpr) => innerExpr.simplify
    case _ => Negate(expr.simplify)
  }

  override def toString = "-(" + expr + ")"
}

case class Add(left: Expression, right: Expression) extends Expression {
  def evaluate(x: Double) = left.evaluate(x) + right.evaluate(x)
  def evaluate(x: Expression) = Add(left.evaluate(x), right.evaluate(x))
  def derive = new Add(left.derive, right.derive)

  override def simplify = (left, right) match {
    case (Zero, expr) => expr.simplify
    case (expr, Zero) => expr.simplify
    case (left, right) => Add(left.simplify, right.simplify)
  }

  override def toString = "(" + left + " + " + right + ")"
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  def evaluate(x: Double) = left.evaluate(x) * right.evaluate(x)
  def evaluate(x: Expression) = Multiply(left.evaluate(x), right.evaluate(x))
  def derive = new Add(new Multiply(left.derive, right), new Multiply(left, right.derive))

  override def simplify = (left, right) match {
    case (Zero, expr) => Zero
    case (expr, Zero) => Zero
    case (One, expr) => expr.simplify
    case (expr, One) => expr.simplify
    case (left, right) => Multiply(left.simplify, right.simplify)
  }
  override def toString = "(" + left + " * " + right + ")"
}

case class Divide(numerator: Expression, denominator: Expression) extends Expression {
  def evaluate(x: Double) = numerator.evaluate(x) / denominator.evaluate(x)
  def evaluate(x: Expression) = Divide(numerator.evaluate(x), denominator.evaluate(x))
  def derive = new Divide(new Add(new Multiply(numerator.derive, denominator), new Negate(new Multiply(numerator, denominator.derive))), new Multiply(denominator, denominator))

  override def simplify = (numerator, denominator) match {
    case (numerator, One) => numerator.simplify
    case _ => Divide(numerator.simplify, denominator.simplify)
  }

  override def toString = numerator.toString + "/" + denominator.toString
}

case class Sine(param: Expression) extends Expression {
  def evaluate(x: Double) = Math.sin(param.evaluate(x))
  def evaluate(x: Expression) = Sine(param.evaluate(x))
  def derive = new Multiply(new Cosine(param), param.derive)

  override def toString = "sin(" + param + ")"
}

case class Cosine(param: Expression) extends Expression {
  def evaluate(x: Double) = Math.cos(param.evaluate(x))
  def evaluate(x: Expression) = Cosine(param.evaluate(x))
  def derive = new Multiply(new Negate(new Sine(param)), param.derive)

  override def toString = "cos(" + param + ")"
}