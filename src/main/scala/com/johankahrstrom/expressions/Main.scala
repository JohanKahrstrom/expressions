package com.johankahrstrom.expressions

object Main {
  def evaluate(e: Expression, x: Double): Double = e match {
    case Zero => 0.0
    case One => 1.0
    case Constant(c) => c
    case Variable => x
    case Add(left, right) => evaluate(left, x) + evaluate(right, x)
    case Negate(expr) => -evaluate(expr, x)
    case Multiply(left, right) => evaluate(left, x) * evaluate(right, x)
    case Divide(numerator, denominator) => evaluate(numerator, x) / evaluate(denominator, x)
    case Sine(param) => Math.sin(evaluate(param, x))
    case Cosine(param) => Math.cos(evaluate(param, x))
  }

  def simplify(e: Expression): Expression = e match {
    case Add(left, right) => (left, right) match {
      case (Zero, expr) => simplify(expr)
      case (expr, Zero) => simplify(expr)
      case (left, right) => Add(simplify(left), simplify(right))
    }
    case Multiply(left, right) => (left, right) match {
      case (Zero, expr) => Zero
      case (expr, Zero) => Zero
      case (One, expr) => simplify(expr)
      case (expr, One) => simplify(expr)
      case (left, right) => Multiply(simplify(left), simplify(right))
    }
    case Divide(numerator, denominator) => Divide(simplify(numerator), simplify(denominator))
    case Negate(expr) => expr match {
      case Negate(innerexpr) => simplify(innerexpr)
      case other => Negate(simplify(expr))
    }
    case expr => expr
  }

  def simplifier(e: Expression): Expression = {
    def loop(acc: Expression): Expression = {
      val simpler = simplify(acc)
      if (acc == simpler) simpler
      else loop(simpler)
    }

    loop(e)
  }

  def toExpression(s: String): Expression = {
    if (s.startsWith("x")) Variable
    else if (s == "x") Variable
    else Zero
  }

  def main(args: Array[String]) {
    val e = toExpression("x * x".replaceAll(" ", ""))
    println(e.toString);
  }
}