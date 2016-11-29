import language._
import language.expressions._
import language.modules.{Binding, Module, ModuleImport}
import language.structs._
import language.types.IntLanguageType
import modes.{ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = Module("hasX", Seq(new Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(new Binding("y", new Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.check(program)
  }

  test("moduleFail") {
    val moduleWithX = Module("hasX", Seq(new Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(new Binding("y", new Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.fail(program)
  }

  test("struct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFail2") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct2"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailBadFieldInit") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", BoolConst(true)))), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct2"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val newStruct = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesStruct = Lambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", Let("takesStruct", takesStruct,
      Application(new Variable("takesStruct"), new Variable("newStruct"))), Some(IntLanguageType))
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = Set(ConstraintHindleyMilner(true)))
  }
}
