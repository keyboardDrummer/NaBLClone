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
    val program = Application(new Lambda("x", Application(new Variable("x"), Const(3)), Some(IntLanguageType)), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("floatingVariable") {
    val program = new Variable("jo")
    assert(!StaticChecker.check(program))
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", new Variable("x"), Some(IntLanguageType)), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside") {
    val program = Application(new Lambda("x", Add(Const(3), new Variable("x")), Some(IntLanguageType)), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside2") {
    val program = Application(new Lambda("x", Add(new Variable("x"), new Variable("x")), Some(IntLanguageType)), Const(3))
    assert(StaticChecker.check(program))
  }

  test("lambdaApplicationAddInside3") {
    val program = Application(new Lambda("x", Add(new Variable("y"), new Variable("x")), Some(IntLanguageType)), Const(3))
    assert(!StaticChecker.check(program))
  }

  test("lambda") {
    val program = new Lambda("x", new Variable("x"), Some(IntLanguageType))
    assert(!StaticChecker.check(program))
  }

  test("addLambda)") {
    val program = Add(Const(3), new Lambda("x", new Variable("x"), Some(IntLanguageType)))
    assert(!StaticChecker.check(program))
  }

  test("lambdaAsArgument)") {
    val identity = new Lambda("x", new Variable("x"), Some(IntLanguageType))
    val functionIdentity = new Lambda("y", new Variable("y"), Some(FunctionLanguageType(IntLanguageType, IntLanguageType)))
    val program = Application(Application(functionIdentity, identity), Const(3))
    assert(StaticChecker.check(program))
  }

  test("Shadowing")  {
    val identity = new Lambda("x", new Variable("x"), Some(IntLanguageType))
    val innerLambda: Lambda = new Lambda("x", new Variable("x"), Some(IntLanguageType))
    val outerLambda: Lambda = new Lambda("x", innerLambda, Some(FunctionLanguageType(IntLanguageType, IntLanguageType)))
    val program = Application(Application(outerLambda, identity), Const(2))
    assert(StaticChecker.check(program))
  }

  test("Shadowing2")  {
    val innerLambda: Lambda = new Lambda("x", new Variable("x"), Some(IntLanguageType))
    val program = Application(Application(new Lambda("x", innerLambda, Some(FunctionLanguageType(IntLanguageType, IntLanguageType))), Const(3)), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("nestedFunction")  {
    val innerLambda: Lambda = new Lambda("y", Add(new Variable("x"), new Variable("y")), Some(IntLanguageType))
    val program = Application(Application(new Lambda("x", innerLambda, Some(IntLanguageType)), Const(3)), Const(2))
    assert(StaticChecker.check(program))
  }
}
