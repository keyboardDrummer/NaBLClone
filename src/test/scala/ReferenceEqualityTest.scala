
import language._
import language.expressions.{Application, Const, Lambda, Variable}
import language.modules.{Binding, Module}
import language.types.{FunctionLanguageType, IntType}
import org.scalatest.FunSuite

class ReferenceEqualityTest extends FunSuite with LanguageWriter {

  test("duplicateReference") {
    val identityType = IntType ==> IntType
    val moduleX = Module("moduleX", Seq(
      Binding("x", Const(3), Some(IntType)),
      Binding("y", Variable("x"), Some(IntType))))
    val moduleY = Module("moduleY", Seq(
      Binding("x", Lambda("y", Const(3), Some(IntType)), Some(identityType)),
      Binding("z", Application(Variable("x"), Const(2)), Some(IntType))))

    val program = Program(Seq(moduleX, moduleY))
    Checker.check(program)
  }
}
