import org.scalatest.FunSuite


class Tests extends FunSuite {

  test("constant") {
    val program = Const(3)
    assert(StaticChecker.check(program))
  }

  test("add") {
    val program = Add(Const(3), Const(2))
    assert(StaticChecker.check(program))
  }

  test("addVariable") {
    val program = Add(Const(3), Variable("jo"))
    assert(!StaticChecker.check(program))
  }

  test("floatingVariable") {
    val program = Variable("jo")
    assert(!StaticChecker.check(program))
  }

  test("lambdaApplication") {
    val program = Application(Lambda("x", IntLanguageType, Variable("x")), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside") {
    val program = Application(Lambda("x", IntLanguageType, Add(Const(3), Variable("x"))), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside2") {
    val program = Application(Lambda("x", IntLanguageType, Add(Variable("x"), Variable("x"))), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside3") {
    val program = Application(Lambda("x", IntLanguageType, Add(Variable("y"), Variable("x"))), Const(3))
    assert(!StaticChecker.check(program))
  }

  test("lambda") {
    val program = Lambda("x", IntLanguageType, Variable("x"))
    assert(!StaticChecker.check(program))
  }

  test("addLambda)") {
    val program = Add(Const(3), Lambda("x", IntLanguageType, Variable("x")))
    assert(!StaticChecker.check(program))
  }

  test("lambdaAsArgument)") {
    val identity = Lambda("x", IntLanguageType, Variable("x"))
    val functionIdentity = Lambda("y", FunctionLanguageType(IntLanguageType, IntLanguageType), Variable("y"))
    val program = Application(Application(functionIdentity, identity), Const(3))
    assert(StaticChecker.check(program))
  }

  test("Shadowing")  {
    val identity = Lambda("x", IntLanguageType, Variable("x"))
    val program = Application(Application(Lambda("x", FunctionLanguageType(IntLanguageType, IntLanguageType), Lambda("x", IntLanguageType, Variable("x"))), identity), Const(2))
    assert(StaticChecker.check(program))
  }

  test("Shadowing2")  {
    val program = Application(Application(Lambda("x", FunctionLanguageType(IntLanguageType, IntLanguageType), Lambda("x", IntLanguageType, Variable("x"))), Const(3)), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("nestedFunction")  {
    val program = Application(Application(Lambda("x", IntLanguageType, Lambda("y", IntLanguageType, Add(Variable("x"), Variable("y")))), Const(3)), Const(2))
    assert(StaticChecker.check(program))
  }
}
