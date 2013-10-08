package com.johankahrstrom.expressions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class EvaluateDoubleSuite extends FunSuite {
  test("Zero is 0.0") {
    assert(Zero.evaluate(10) === 0.0)
  }

  test("One is 1.0") {
    assert(One.evaluate(20) === 1.0)
  }

  test("Constant is constant") {
    assert(Constant(1.0).evaluate(2.0) === 1.0)
    assert(Constant(8.0).evaluate(3.0) === 8.0)
  }

  test("Variable is variable") {
    assert(Variable.evaluate(1.0) === 1.0)
    assert(Variable.evaluate(Math.PI) === Math.PI)
  }

  test("Negation negates") {
    assert(Negate(Constant(2.0)).evaluate(Math.PI) === -2.0)
    assert(Negate(Variable).evaluate(-8.0) === 8.0)
  }

  test("Addition adds") {
    assert(Add(Constant(2.0), Constant(4.0)).evaluate(1.0) === 6.0)
    assert(Add(Constant(-2.0), Variable).evaluate(4.0) === 2.0)
  }

  test("Multiplication multiplies") {
    assert(Multiply(Constant(2.0), Constant(4.0)).evaluate(2.0) === 8.0)
    assert(Multiply(Constant(4.0), Variable).evaluate(2.0) === 8.0)
  }

  test("Division divides") {
    assert(Divide(Constant(4.0), Constant(2.0)).evaluate(200.0) === 2.0)
    assert(Divide(Constant(8.0), Variable).evaluate(2.0) === 4.0)
  }

  test("Sine is sine") {
    assert(Sine(Constant(Math.PI / 3.0)).evaluate(200.0) === Math.sin(Math.PI / 3.0))
    assert(Sine(Variable).evaluate(Math.PI) === Math.sin(Math.PI))
  }

  test("Cosine is cosine") {
    assert(Cosine(Constant(Math.PI / 3.0)).evaluate(100.0) === Math.cos(Math.PI / 3.0))
    assert(Cosine(Variable).evaluate(Math.PI) === Math.cos(Math.PI))
  }
}