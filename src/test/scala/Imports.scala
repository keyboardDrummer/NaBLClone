import constraints.StaticChecker
import language._
import language.expressions._
import language.modules.{Binding, Module, ModuleImport}
import language.structs._
import language.types.IntLanguageType
import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = Module("hasX", Seq(new Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(new Binding("y", new Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.both(program))
  }

  test("moduleFail") {
    val moduleWithX = Module("hasX", Seq(new Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(new Binding("y", new Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.both(program))
  }

  test("struct") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("structFail") {
    val structNew = new Binding("newStruct", new New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("structFail2") {
    val structNew = new Binding("newStruct", new New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct2"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("lambdaTakingStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val newStruct = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesStruct = new Lambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Let("takesStruct", takesStruct,
          Application(new Variable("takesStruct"), new Variable("newStruct"))), Some(IntLanguageType))
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }
}
