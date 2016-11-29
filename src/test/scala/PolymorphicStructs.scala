
import language.{LanguageWriter, Program}
import language.expressions.{BoolConst, Const, Variable}
import language.modules.{Binding, Module}
import language.structs.{StructFieldInit, _}
import language.types._
import org.scalatest.FunSuite

class PolymorphicStructs extends FunSuite with LanguageWriter {

  test("struct") {
    val structDeclaration = Struct("s", Seq("x" of "a"), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq("x" is 3), genericTypeArgument = Some(IntLanguageType)))
    val structUse = Binding("structUse", Variable("newStruct").access("x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailWrongAccessType") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailMixedTypeVariablesInDeclaration") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("b"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailBadFieldInit") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(IntLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("reuseStruct") {
    val structDeclaration = Struct("s", Seq(Field("x", _type = "a")), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", 3)), genericTypeArgument = Some(IntLanguageType)))
    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", true)), genericTypeArgument = Some(BoolLanguageType)))
    val structUse = Binding("structUse", Variable("newStruct").access("x"), bindingType = Some(IntLanguageType))
    val structUse2 = Binding("structUse2", Variable("newStruct2").access("x"), bindingType = Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("reuseStructFail") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(BoolLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolLanguageType))
    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("reuseStructFail2") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(BoolLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailBadFieldInit2") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("reuseStructFailBadFieldInit") {
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)))
    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolLanguageType)))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolLanguageType))
    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  ignore("structNewWithStaticType") {
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntLanguageType)),
      Some(LanguageTypeApplication(new LanguageStructType("s"), IntLanguageType)))
    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }
}
