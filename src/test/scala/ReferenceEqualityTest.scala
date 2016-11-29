
import language._
import language.expressions.{Application, Const, Lambda, Variable}
import language.modules.{Binding, Module}
import language.types.{FunctionLanguageType, IntLanguageType}
import org.scalatest.FunSuite

class ReferenceEqualityTest extends FunSuite {

  test("duplicateReference") {
    val identityType = FunctionLanguageType(IntLanguageType, IntLanguageType)
    val moduleX = Module("moduleX", Seq(
      Binding("x", Const(3), Some(IntLanguageType)),
      Binding("y", Variable("x"), Some(IntLanguageType))))
    val moduleY = Module("moduleY", Seq(
      Binding("x", Lambda("y", Const(3), Some(IntLanguageType)), Some(identityType)),
      Binding("z", Application(Variable("x"), Const(2)), Some(IntLanguageType))))

    val program = Program(Seq(moduleX, moduleY))
    Checker.check(program)
  }
}
