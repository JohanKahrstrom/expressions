package com.johankahrstrom.expressions

import scala.math.Numeric.DoubleIsFractional
import scala.annotation.tailrec

trait Expression[T] {
  def evaluate(x: T): T
  def evaluate(x: Expression[T]): Expression[T]
  def derive: Expression[T]
  
  // Default implementation of simplify is to return the expression unchanged
  def simplify = this
}

object Expression {
  @tailrec
  def simplify[T](exp: Expression[T]): Expression[T] = {
    val simpler = exp.simplify
    if (simpler == exp) exp
    else simplify(simpler)
  }
}

object Constant {
  def unapply[T](c: Constant[T]): Option[T] = Some(c.evaluate)
  def apply[T](c: T)(implicit n: Numeric[T]): Constant[T] = new Constant[T](c)
}

class Constant[T](c: T)(implicit n: Numeric[T]) extends Expression[T] {
  def evaluate: T = c
  def evaluate(x: T) = c
  def evaluate(x: Expression[T]) = this
  def derive = Zero[T]

  override def toString = c.toString
}

case class Zero[T]()(implicit n: Numeric[T]) extends Constant[T](n.zero) {
  override def toString = "0"
}

case class One[T]()(implicit n: Numeric[T]) extends Constant[T](n.one) {
  override def toString = "1"
}

case class Variable[T](implicit n: Numeric[T]) extends Expression[T] {
  def evaluate(x: T) = x
  def evaluate(x: Expression[T]) = x
  def derive = One[T]

  override def toString = "x"
}

case class Negate[T](expr: Expression[T])(implicit n: Numeric[T]) extends Expression[T] {
  def evaluate(x: T): T = n.negate(expr.evaluate(x))
  def evaluate(x: Expression[T]) = Negate(expr.evaluate(x))
  def derive = new Negate(expr.derive)

  override def simplify = expr match {
    case Negate(innerExpr) => innerExpr.simplify
    case _ => Negate(expr.simplify)
  }

  override def toString = "-(" + expr + ")"
}

case class Add[T](left: Expression[T], right: Expression[T])(implicit n: Numeric[T]) extends Expression[T] {
  def evaluate(x: T) = n.plus(left.evaluate(x), right.evaluate(x))
  def evaluate(x: Expression[T]) = Add(left.evaluate(x), right.evaluate(x))
  def derive = new Add(left.derive, right.derive)

  override def simplify = (left, right) match {
    case (Zero(), expr) => expr.simplify
    case (expr, Zero()) => expr.simplify
    case (Constant(c1), Constant(c2)) => Constant(n.plus(c1, c2))
    case (left, right) => Add(left.simplify, right.simplify)
  }

  override def toString = "(" + left + " + " + right + ")"
}

case class Multiply[T](left: Expression[T], right: Expression[T])(implicit n: Numeric[T]) extends Expression[T] {
  def evaluate(x: T) = n.times(left.evaluate(x), right.evaluate(x))
  def evaluate(x: Expression[T]) = Multiply(left.evaluate(x), right.evaluate(x))
  def derive = new Add(new Multiply(left.derive, right), new Multiply(left, right.derive))

  override def simplify = (left, right) match {
    case (Zero(), expr) => Zero()
    case (expr, Zero()) => Zero()
    case (Constant(c1), Constant(c2)) => Constant(n.times(c1, c2))
    case (One(), expr) => expr.simplify
    case (expr, One()) => expr.simplify
    case (left, right) => Multiply(left.simplify, right.simplify)
  }
  override def toString = "(" + left + " * " + right + ")"
}

case class Divide[T](numerator: Expression[T], denominator: Expression[T])(implicit n: Fractional[T]) extends Expression[T] {
  def evaluate(x: T) = n.div(numerator.evaluate(x), denominator.evaluate(x))
  def evaluate(x: Expression[T]) = Divide(numerator.evaluate(x), denominator.evaluate(x))
  def derive = new Divide(new Add(new Multiply(numerator.derive, denominator), new Negate(new Multiply(numerator, denominator.derive))), new Multiply(denominator, denominator))

  override def simplify = (numerator, denominator) match {
    case (Zero(), denominator) => Zero()
    case (numerator, One()) => numerator.simplify
    case _ => Divide(numerator.simplify, denominator.simplify)
  }

  override def toString = numerator.toString + "/" + denominator.toString
}

// From here on, we require T = Double

abstract class Function[T](param: Expression[T])(implicit n: Numeric[T]) extends Expression[T] {
  def functionDerivate: Expression[T]
  def derive = new Multiply[T](functionDerivate, param.derive)
}

case class Sine(param: Expression[Double]) extends Function[Double](param) {
  def evaluate(x: Double) = Math.sin(param.evaluate(x))
  def evaluate(x: Expression[Double]) = Sine(param.evaluate(x))
  def functionDerivate = new Cosine(param)

  override def toString = "sin(" + param + ")"
}

case class Cosine(param: Expression[Double]) extends Function[Double](param) {
  def evaluate(x: Double) = Math.cos(param.evaluate(x))
  def evaluate(x: Expression[Double]) = Cosine(param.evaluate(x))
  def functionDerivate = new Negate(new Sine(param))

  override def toString = "cos(" + param + ")"
}