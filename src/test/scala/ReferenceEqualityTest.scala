import constraints.StaticChecker
import language._
import language.expressions.{Application, Const, Lambda, Variable}
import language.modules.{Binding, Module}
import language.types.{FunctionLanguageType, IntLanguageType}
import org.scalatest.FunSuite

class ReferenceEqualityTest extends FunSuite {

  test("duplicateReference") {
    val identityType = FunctionLanguageType(IntLanguageType, IntLanguageType)
    val moduleX = new Module("moduleX", Seq(
      new Binding("x", IntLanguageType, Const(3)),
      new Binding("y", IntLanguageType, new Variable("x"))))
    val moduleY = new Module("moduleY", Seq(
      new Binding("x", identityType, new Lambda("y", Const(3), Some(IntLanguageType))),
      new Binding("z", IntLanguageType, Application(new Variable("x"), Const(2)))))

    val program = Program(Seq(moduleX, moduleY))
    assert(StaticChecker.check(program))
  }
}
