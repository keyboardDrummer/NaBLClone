import constraints.StaticChecker
import language._
import language.expressions._
import language.modules.{Binding, Module, ModuleImport}
import language.structs._
import language.types.IntLanguageType
import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(StaticChecker.both(program))
  }

  test("moduleFail") {
    val moduleWithX = new Module("hasX", Seq(new Binding("x", IntLanguageType, Const(3))))
    val moduleThatImportsHasX = new Module("importsHasX", Seq(new Binding("y", IntLanguageType, new Variable("x"))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    assert(!StaticChecker.both(program))
  }

  test("struct") {
    val structNew = new Binding("newStruct", new LanguageStructType("s"), new New("s", Seq(new StructFieldInit("x", Const(3)))))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("structFail") {
    val structNew = new Binding("newStruct", new LanguageStructType("s"), new New("s2", Seq(new StructFieldInit("x", Const(3)))))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("structFail2") {
    val structNew = new Binding("newStruct", new LanguageStructType("s"), new New("s2", Seq(new StructFieldInit("x", Const(3)))))
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct2"), "x"))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("lambdaTakingStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val newStruct = new Binding("newStruct", new LanguageStructType("s"), new New("s", Seq(new StructFieldInit("x", Const(3)))))
    val takesStruct = new Lambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", IntLanguageType, new Let("takesStruct", takesStruct,
      Application(new Variable("takesStruct"), new Variable("newStruct"))))
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }
}
