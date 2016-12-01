import language.LanguageWriter
import language.expressions._
import language.types.{FunctionLanguageType, IntType}
import modes.{ConstraintClosure, ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class BasicTests extends FunSuite with LanguageWriter {

  test("constant") {
    val program = Const(3)
    Checker.checkExpression(program)
  }

  test("add") {
    val program = Add(3, 2)
    Checker.checkExpression(program)
  }

  test("addVariable") {
    val program = Add(3, "jo")
    Checker.failExpression(program)
  }

  test("badArgumentUse") {
    val program = Lambda("x", Variable("x") $ 3, Some(IntType)) $ 2
    Checker.failExpression(program)
  }

  test("floatingVariable") {
    val program = Variable("jo")
    Checker.failExpression(program)
  }

  test("lambdaApplication") {
    val program = Lambda("x", "x", Some(IntType)) $ 3
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside") {
    val program = Lambda("x", 3 add "x", Some(IntType)) $ 3
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside2") {
    val program = Lambda("x", "x" add "x", Some(IntType)) $ 3
    Checker.checkExpression(program)
  }

  test("lambdaApplicationAddInside3") {
    val program = Lambda("x", Add("y", "x"), Some(IntType)) $ 3
    Checker.failExpression(program)
  }

  test("lambda") {
    val program = Lambda("x", Variable("x"), Some(IntType))
    Checker.failExpression(program)
  }

  test("addLambda)") {
    val program = Add(Const(3), Lambda("x", Variable("x"), Some(IntType)))
    Checker.failExpression(program)
  }

  test("lambdaAsArgument)") {
    val identity = Lambda("x", "x", Some(IntType))
    val functionIdentity = Lambda("y", "y", Some(IntType ==> IntType))
    val program = functionIdentity $ identity $ 3
    Checker.checkExpression(program)
  }

  test("Shadowing")  {
    val identity = Lambda("x", Variable("x"), Some(IntType))
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntType))
    val outerLambda: Lambda = Lambda("x", innerLambda, Some(IntType ==> IntType))
    val program = Application(Application(outerLambda, identity), Const(2))
    Checker.checkExpression(program, skip = Set(ConstraintHindleyMilner(true)))
  }

  test("Shadowing2")  {
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntType))
    val program = Application(Application(Lambda("x", innerLambda, Some(IntType ==> IntType)), Const(3)), Const(2))
    Checker.failExpression(program)
  }

  test("Shadowing3")  {
    val innerLambda: Lambda = Lambda("x", Variable("x"), Some(IntType))
    val program = Application(Application(Lambda("x", innerLambda, Some(IntType ==> IntType)), Const(3)), Const(2))
    Checker.failExpression(program)
  }

  test("nestedFunction")  {
    val innerLambda: Lambda = Lambda("y", Add(Variable("x"), Variable("y")), Some(IntType))
    val program = Application(Application(Lambda("x", innerLambda, Some(IntType)), Const(3)), Const(2))
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

  test("recursive function")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x")), "f" $ 0)
    Checker.checkExpression(incrementInfinitely, skip = Set(MachineChecker, ConstraintClosure) ++ ConstraintHindleyMilner.both)
  }

  test("recursive function typed let")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x")), "f" $ 0, bindingLanguageType = Some(IntType ==> IntType))
    Checker.checkExpression(incrementInfinitely)
  }

  test("recursive function typed lambda")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x"), parameterDefinedType = Some(IntType)), "f" $ 0)
    Checker.checkExpression(incrementInfinitely, skip = Set(ConstraintClosure, MachineChecker, ConstraintHindleyMilner(true)))
  }
}
