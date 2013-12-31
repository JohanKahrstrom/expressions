package com.johankahrstrom.expressions

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class DeriveSuite extends FunSuite {
  test("derived zero is zero") {
    assert(Zero[Double].derive === Zero[Double])
  }

  test("derived constant is zero") {
    assert(Constant(5.0).derive === Zero[Double])
    assert(Constant(0.0).derive === Zero[Double])
  }

  test("derived variable is one") {
    assert(Variable[Double].derive === One[Double])
  }
  
  test("d/dx x^n = nx^(n-1)") {
    val exp = new Multiply(Variable[Int], Variable[Int])
    assert(Expression.simplify(exp.derive) === Add(Variable[Int], Variable[Int]))
  }
}