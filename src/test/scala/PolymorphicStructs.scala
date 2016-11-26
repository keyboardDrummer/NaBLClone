import constraints.StaticChecker
import language.Program
import language.expressions.{Const, NoSpecializeVariable, Variable}
import language.modules.{Binding, Module}
import language.structs._
import language.types.{BoolLanguageType, IntLanguageType, LanguageTypeApplication, LanguageTypeVariable}
import org.scalatest.FunSuite

class PolymorphicStructs extends FunSuite {

  test("struct") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new NoSpecializeVariable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("reuseStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structNew2 = new Binding("newStruct2", new New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Access(new NoSpecializeVariable("newStruct"), "x"), Some(IntLanguageType))
    val structUse2 = new Binding("structUse2", new Access(new NoSpecializeVariable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("struct2") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new NoSpecializeVariable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("structFail") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }

  test("structFail2") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }

  test("structFail3") {
    val structNew = new Binding("newStruct", new New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("b"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.checkMachine(program))
  }
}
