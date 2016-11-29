import language._
import language.expressions._
import language.modules.{Binding, Module, ModuleImport}
import language.structs._
import language.types.IntLanguageType
import modes.{ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class Imports extends FunSuite {

  test("module") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.check(program)
  }

  test("moduleFail") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntLanguageType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntLanguageType))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.fail(program)
  }

  test("struct") {
    val structDeclaration = Struct("s", Seq(Field("x", IntLanguageType)))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structDeclaration = Struct("s", Seq(Field("x", IntLanguageType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFail2") {
    val structDeclaration = Struct("s", Seq(Field("x", IntLanguageType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct2"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailBadFieldInit") {
    val structDeclaration = Struct("s", Seq(Field("x", IntLanguageType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", BoolConst(true)))), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct2"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingStruct") {
    val structDeclaration = Struct("s", Seq(Field("x", IntLanguageType)))
    val newStruct = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesStruct", takesStruct,
      Application(Variable("takesStruct"), Variable("newStruct"))), Some(IntLanguageType))
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = Set(ConstraintHindleyMilner(true)))
  }
}
