import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("basic") {
    val moduleWithX = Module("hasX", Seq(Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", IntLanguageType, Variable("x"))), imports = Seq(ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.check(program))
  }

  test("basicFail") {
    val moduleWithX = Module("hasX", Seq(Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", IntLanguageType, Variable("x"))), imports = Seq(ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.check(program))
  }
}
