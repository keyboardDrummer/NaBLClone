import constraints.StaticChecker
import language.Program
import language.expressions.{Const, Variable}
import language.modules.{Binding, Module}
import language.structs._
import language.types.{BoolLanguageType, IntLanguageType, LanguageTypeVariable, TypeApplication}
import org.scalatest.FunSuite

class PolymorphicStructs extends FunSuite {

  test("struct") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      TypeApplication(new LanguageStructType("s"), IntLanguageType))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), IntLanguageType)
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.checkMachine(program))
  }

  test("structFail") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)),
      TypeApplication(new LanguageStructType("s"), IntLanguageType))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), IntLanguageType)
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }

  test("structFail2") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      TypeApplication(new LanguageStructType("s"), IntLanguageType))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), BoolLanguageType)
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }

  test("structFail3") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      TypeApplication(new LanguageStructType("s"), IntLanguageType))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("b"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), IntLanguageType)
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }
}
