package com.johankahrstrom.expressions

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class DeriveSuite extends FunSuite {
  test("derived zero is zero") {
    assert(Zero.derive == Zero)
  }

  test("derived constant is zero") {
    assert(Constant(5).derive == Zero)
    assert(Constant(0).derive == Zero)
  }

  test("derived variable is one") {
    assert(Variable.derive == One)
  }
}