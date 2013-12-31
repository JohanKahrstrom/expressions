package com.johankahrstrom.expressions

import scala.annotation.tailrec

object Main {
  type Exp = Expression[Double]
  def simplifier(e: Exp): Exp = {
    @tailrec
    def loop(acc: Exp): Exp = {
      val simpler = acc.simplify
      if (acc == simpler) simpler
      else loop(simpler)
    }

    loop(e)
  }

  def toExpression(s: String): Exp = {
    if (s.startsWith("x")) Variable[Double]
    else if (s == "x") Variable[Double]
    else Zero[Double]
  }

  def main(args: Array[String]) {
    val ex = new Cosine(Variable[Double])
    println(ex.derive.derive)
    println(Expression.simplify(ex.derive.derive))
    
    println()
    println()
    
    val e = toExpression("x * x".replaceAll(" ", ""))
    println(e.toString);
    println(ex.derive.derive.derive)
    println(ex.derive.derive.derive.simplify)
    println(Expression.simplify(ex.derive.derive.derive))
    
    println()
    println()
    
    val ex2 = new Multiply(Variable[Int], Add(Constant(2), Variable[Int]))
    println(ex2)
    println(ex2.derive)
    println(ex2.derive.derive)
    println(ex2.derive.derive.derive)
    println(Expression.simplify(ex2))
    println(Expression.simplify(ex2.derive))
    println(Expression.simplify(ex2.derive.derive))
    println(Expression.simplify(ex2.derive.derive.derive))
  }
}