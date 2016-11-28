
import language.Program
import language.expressions.{BoolConst, Const, Variable}
import language.modules.{Binding, Module}
import language.structs.{StructFieldInit, _}
import language.types._
import org.scalatest.FunSuite

class PolymorphicStructs extends FunSuite {

  test("struct") {
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Variable("newStruct").access("x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.check(program)
  }

  test("structFail") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(BoolLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("structFailWrongAccessType") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("structFailMixedTypeVariablesInDeclaration") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("b"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("structFailBadFieldInit") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", BoolConst(true))), Some(IntLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("reuseStruct") {
    val structDeclaration = new Struct("s", Seq(new Field("x", _type = "a")), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", 3)), genericTypeArgument = Some(IntLanguageType)))
    val structNew2 = new Binding("newStruct2", new New("s", Seq(new StructFieldInit("x", true)), genericTypeArgument = Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Variable("newStruct").access("x"), bindingType = Some(IntLanguageType))
    val structUse2 = new Binding("structUse2", new Variable("newStruct2").access("x"), bindingType = Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.check(program)
  }

  test("reuseStructFail") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structNew2 = new Binding("newStruct2", new New("s", Seq(new StructFieldInit("x", BoolConst(true))), Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(BoolLanguageType))
    val structUse2 = new Binding("structUse2", new Access(new Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("reuseStructFail2") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structNew2 = new Binding("newStruct2", new New("s", Seq(new StructFieldInit("x", BoolConst(true))), Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val structUse2 = new Binding("structUse2", new Access(new Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("structFailBadFieldInit2") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  test("reuseStructFailBadFieldInit") {
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structNew2 = new Binding("newStruct2", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val structUse2 = new Binding("structUse2", new Access(new Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.fail(program)
  }

  ignore("structNewWithStaticType") {
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = new Struct("s", Seq(new Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    StaticChecker.check(program)
  }
}
