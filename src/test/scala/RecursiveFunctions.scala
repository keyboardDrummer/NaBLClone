import language.LanguageWriter
import language.expressions.{Add, Lambda, Let}
import language.types.IntType
import modes.{ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class RecursiveFunctions extends FunSuite with LanguageWriter {

  test("recursive function")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x")), "f" $ 0)
    Checker.checkExpression(incrementInfinitely, skip = Set(MachineChecker) ++ ConstraintHindleyMilner.both)
  }

  test("recursive function typed let")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x")), "f" $ 0, bindingLanguageType = Some(IntType ==> IntType))
    Checker.checkExpression(incrementInfinitely)
  }

  test("recursive function typed lambda")
  {
    val incrementInfinitely = Let("f", Lambda("x", Add(1, "f" $ "x"), parameterDefinedType = Some(IntType)), "f" $ 0)
    Checker.checkExpression(incrementInfinitely, skip = Set(MachineChecker, ConstraintHindleyMilner(true)))
  }
}
