import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("basic") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.check(program))
  }

  test("basicFail") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.check(program))
  }
}
