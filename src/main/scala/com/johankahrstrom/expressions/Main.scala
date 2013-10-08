package com.johankahrstrom.expressions

object Main {
  def simplify(e: Expression): Expression = e match {
    case a: Add => a.simplify
    case m: Multiply => m.simplify
    case Divide(numerator, denominator) => Divide(simplify(numerator), simplify(denominator))
    case Negate(expr) => expr match {
      case Negate(innerexpr) => simplify(innerexpr)
      case other => Negate(simplify(expr))
    }
    case expr => expr
  }

  def simplifier(e: Expression): Expression = {
    def loop(acc: Expression): Expression = {
      val simpler = acc.simplify
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
    val ex = new Cosine(Variable)
    println(ex.derive.derive)
    println(simplifier(ex.derive.derive.derive.derive))
    val e = toExpression("x * x".replaceAll(" ", ""))
    println(e.toString);
    println(ex.derive.derive.derive)
    println(ex.derive.derive.derive.simplify)
    println(Expression.simplify(ex.derive.derive.derive))
  }
}