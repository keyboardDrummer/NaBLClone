import constraints.StaticChecker
import language._
import language.expressions._
import language.modules.{Binding, Module, ModuleImport}
import language.structs._
import language.types.IntLanguageType
import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", Const(3), IntLanguageType)))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", new Variable("x"), IntLanguageType)), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.both(program))
  }

  test("moduleFail") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", Const(3), IntLanguageType)))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", new Variable("x"), IntLanguageType)), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.both(program))
  }

  test("struct") {
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3)))), new LanguageStructType("s"))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), IntLanguageType)
    val module = new Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("structFail") {
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), new LanguageStructType("s"))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), IntLanguageType)
    val module = new Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("structFail2") {
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), new LanguageStructType("s"))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct2"), "x"), IntLanguageType)
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("lambdaTakingStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val newStruct = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3)))), new LanguageStructType("s"))
    val takesStruct = new Lambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Let("takesStruct", takesStruct,
          Application(new Variable("takesStruct"), new Variable("newStruct"))), IntLanguageType)
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }
}
