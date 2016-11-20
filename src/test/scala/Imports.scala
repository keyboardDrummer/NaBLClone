import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.check(program))
  }

  test("moduleFail") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.check(program))
  }

  test("struct") {
    val structNew = new Binding("newStruct", new LanguageStructType("s"), new New("s", Seq(new StructFieldInit("x", Const(3)))))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }
}
