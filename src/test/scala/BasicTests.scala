import language.expressions._
import language.types.{FunctionLanguageType, IntLanguageType}
import modes.{ConstraintClosure, ConstraintHindleyMilner}
import org.scalatest.FunSuite

class BasicTests extends FunSuite {

  test("constant") {
    val program = Const(3)
    Checker.checkExpression(program)
  }

  test("add") {
    val program = Add(Const(3), Const(2))
    Checker.checkExpression(program)
  }

  test("addVariable") {
    val program = Add(Const(3), Variable("jo"))
    Checker.failExpression(program)
  }

  test("badArgumentUse") {
    val program = Application(Lambda("x", Application(Variable("x"), Const(3)), Some(IntLanguageType)), Const(2))
    Checker.failExpression(program)
  }

  test("floatingVariable") {
    val program = Variable("jo")
    Checker.failExpression(program)
  }

  test("lambdaApplication") {
    val program = Application(Lambda("x", Variable("x"), Some(IntLanguageType)), Const(3))
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside") {
    val program = Application(Lambda("x", Add(Const(3), Variable("x")), Some(IntLanguageType)), Const(3))
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside2") {
    val program = Application(Lambda("x", Add(Variable("x"), Variable("x")), Some(IntLanguageType)), Const(3))
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside3") {
    val program = Application(Lambda("x", Add(Variable("y"), Variable("x")), Some(IntLanguageType)), Const(3))
    Checker.failExpression(program)
  }

  test("lambda") {
    val program = Lambda("x", Variable("x"), Some(IntLanguageType))
    Checker.failExpression(program)
  }

  test("addLambda)") {
    val program = Add(Const(3), Lambda("x", Variable("x"), Some(IntLanguageType)))
    Checker.failExpression(program)
  }

  test("lambdaAsArgument)") {
    val identity = Lambda("x", Variable("x"), Some(IntLanguageType))
    val functionIdentity = Lambda("y", Variable("y"), Some(FunctionLanguageType(IntLanguageType, IntLanguageType)))
    val program = Application(Application(functionIdentity, identity), Const(3))
    Checker.checkExpression(program)
  }

  test("Shadowing")  {
    val identity = Lambda("x", Variable("x"), Some(IntLanguageType))
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntLanguageType))
    val outerLambda: Lambda = Lambda("x", innerLambda, Some(FunctionLanguageType(IntLanguageType, IntLanguageType)))
    val program = Application(Application(outerLambda, identity), Const(2))
    Checker.checkExpression(program, skip = Set(ConstraintHindleyMilner(true)))
  }

  test("Shadowing2")  {
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntLanguageType))
    val program = Application(Application(Lambda("x", innerLambda, Some(FunctionLanguageType(IntLanguageType, IntLanguageType))), Const(3)), Const(2))
    Checker.failExpression(program)
  }

  test("Shadowing3")  {
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntLanguageType))
    val program = Application(Application(Lambda("x", innerLambda, Some(FunctionLanguageType(IntLanguageType, IntLanguageType))), Const(3)), Const(2))
    Checker.failExpression(program)
  }

  test("nestedFunction")  {
    val innerLambda: Lambda = Lambda("y", Add(Variable("x"), Variable("y")), Some(IntLanguageType))
    val program = Application(Application(Lambda("x", innerLambda, Some(IntLanguageType)), Const(3)), Const(2))
    Checker.checkExpression(program)
  }

  test("let") {
    val program = Let("x", Const(3), Variable("x"))
    Checker.checkExpression(program)
  }

  test("letTwice") {
    val program = Let("x", Const(3), Add(Variable("x"), Variable("x")))
    Checker.checkExpression(program)
  }

  test("letFail") {
    val program = Let("x", Const(3), Variable("y"))
    Checker.failExpression(program)
  }
}
