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
    println("new Cosine(Variable[Double])")
    val ex = new Cosine(Variable[Double])
    println(ex.derive.derive)
    println(Expression.simplify(ex.derive.derive))
    
    println()
    println()
    println("toExpression(\"x * x\".replaceAll(\" \", \"\"))")
    val e = toExpression("x * x".replaceAll(" ", ""))
    println(e.toString);

    println()
    println()
    println("new Multiply(Variable[Int], Add(Constant(2), Variable[Int]))")
    val ex2 = new Multiply(Variable[Int], Add(Constant(2), Variable[Int]))
    println(ex2)
    println(ex2.derive)
    println(ex2.derive.derive)
    println(ex2.derive.derive.derive)
    println(Expression.simplify(ex2))
    println(Expression.simplify(ex2.derive))
    println(Expression.simplify(ex2.derive.derive))
    println(Expression.simplify(ex2.derive.derive.derive))
    println(ex2.evaluate(ex2))

    println()
    println()
    println("new Add(Constant(3), Constant(8))")
    val ex3 = new Add(Constant(3), Constant(8))
    println(ex3)
    println(ex3.simplify)
  }
}