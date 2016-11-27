import constraints.StaticChecker
import language._
import language.expressions.{Application, Const, Lambda, Variable}
import language.modules.{Binding, Module}
import language.types.{FunctionLanguageType, IntLanguageType}
import org.scalatest.FunSuite

class ReferenceEqualityTest extends FunSuite {

  test("duplicateReference") {
    val identityType = FunctionLanguageType(IntLanguageType, IntLanguageType)
    val moduleX = Module("moduleX", Seq(
      new Binding("x", Const(3), Some(IntLanguageType)),
      new Binding("y", new Variable("x"), Some(IntLanguageType))))
    val moduleY = Module("moduleY", Seq(
      new Binding("x", new Lambda("y", Const(3), Some(IntLanguageType)), Some(identityType)),
      new Binding("z", Application(new Variable("x"), Const(2)), Some(IntLanguageType))))

    val program = Program(Seq(moduleX, moduleY))
    assert(StaticChecker.check(program))
  }
}
