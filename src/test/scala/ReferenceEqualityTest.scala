import org.scalatest.FunSuite

class ReferenceEqualityTest extends FunSuite {

  test("duplicateReference") {
    val identityType = FunctionLanguageType(IntLanguageType, IntLanguageType)
    val moduleX = Module("moduleX", Seq(
      Binding("x", IntLanguageType, Const(3)),
      Binding("y", IntLanguageType, Variable("x"))))
    val moduleY = Module("moduleY", Seq(
      Binding("x", identityType, Lambda("y", IntLanguageType, Const(3))),
      Binding("z", IntLanguageType, Application(Variable("x"), Const(2)))))

    val program = Program(Seq(moduleX, moduleY))
    assert(StaticChecker.check(program))
  }
}
