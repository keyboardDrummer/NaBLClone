import org.scalatest.FunSuite


class BasicTests extends FunSuite {

  test("constant") {
    val program = Const(3)
    assert(StaticChecker.check(program))
  }

  test("add") {
    val program = Add(Const(3), Const(2))
    assert(StaticChecker.check(program))
  }

  test("addVariable") {
    val program = Add(Const(3), new Variable("jo"))
    assert(!StaticChecker.check(program))
  }

  test("badArgumentUse") {
    val program = Application(new Lambda("x", IntLanguageType, Application(new Variable("x"), Const(3))), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("floatingVariable") {
    val program = new Variable("jo")
    assert(!StaticChecker.check(program))
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", IntLanguageType, new Variable("x")), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside") {
    val program = Application(new Lambda("x", IntLanguageType, Add(Const(3), new Variable("x"))), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside2") {
    val program = Application(new Lambda("x", IntLanguageType, Add(new Variable("x"), new Variable("x"))), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside3") {
    val program = Application(new Lambda("x", IntLanguageType, Add(new Variable("y"), new Variable("x"))), Const(3))
    assert(!StaticChecker.check(program))
  }

  test("lambda") {
    val program = new Lambda("x", IntLanguageType, new Variable("x"))
    assert(!StaticChecker.check(program))
  }

  test("addLambda)") {
    val program = Add(Const(3), new Lambda("x", IntLanguageType, new Variable("x")))
    assert(!StaticChecker.check(program))
  }

  test("lambdaAsArgument)") {
    val identity = new Lambda("x", IntLanguageType, new Variable("x"))
    val functionIdentity = new Lambda("y", FunctionLanguageType(IntLanguageType, IntLanguageType), new Variable("y"))
    val program = Application(Application(functionIdentity, identity), Const(3))
    assert(StaticChecker.check(program))
  }

  test("Shadowing")  {
    val identity = new Lambda("x", IntLanguageType, new Variable("x"))
    val program = Application(Application(new Lambda("x", FunctionLanguageType(IntLanguageType, IntLanguageType),
      new Lambda("x", IntLanguageType, new Variable("x"))), identity), Const(2))
    assert(StaticChecker.check(program))
  }

  test("Shadowing2")  {
    val program = Application(Application(new Lambda("x", FunctionLanguageType(IntLanguageType, IntLanguageType), new Lambda("x", IntLanguageType, new Variable("x"))), Const(3)), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("nestedFunction")  {
    val program = Application(Application(new Lambda("x", IntLanguageType, new Lambda("y", IntLanguageType, Add(new Variable("x"), new Variable("y")))), Const(3)), Const(2))
    assert(StaticChecker.check(program))
  }
}
