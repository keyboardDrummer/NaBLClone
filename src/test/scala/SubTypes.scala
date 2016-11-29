
import language.Program
import language.expressions._
import language.modules.{Binding, Module}
import language.structs._
import language.types.{IntLanguageType, LongLanguageType}
import modes.{ConstraintClosure, ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class SubTypes extends FunSuite {

  test("intAddition") {
    val program = OverloadedAdd(Const(3), Const(2))
    Checker.checkExpression(program, IntLanguageType)
  }

  test("longAddition") {
    val program = OverloadedAdd(LongConst(3), LongConst(2))
    Checker.checkExpression(program, LongLanguageType)
  }

  test("struct") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(), Some("s3"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFail2") {
    val structParent = Struct("s", Seq(Field("y", IntLanguageType)))
    val structChild = Struct("s2", Seq(), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structBigger") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(Field("y", IntLanguageType)), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val structUse = Binding("structUse", Add(Access(Variable("newStruct"), "x"), Access(Variable("newStruct"), "y")), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structBiggerFail") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(Field("y", IntLanguageType)), Some("s"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s")))
    val module = Module("module", Seq(structNew), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("longLambdaTakesInt") {
    val program = Application(Lambda("x", Variable("x"), Some(LongLanguageType)), Const(3))
    Checker.checkExpression(program, IntLanguageType, skip = Set(ConstraintHindleyMilner(false)))
  }

  test("longLambdaTakesIntFail") {
    val program = Application(Lambda("x", Variable("x"), Some(LongLanguageType)), Const(3))
    Checker.failExpression(program, LongLanguageType)
  }

  test("intLambdaTakesLong") {
    val program = Application(Lambda("x", Variable("x"), Some(IntLanguageType)), LongConst(3))
    Checker.failExpression(program)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismLambda") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = ConstraintHindleyMilner.both)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismLambdaFail") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq())
    val takesSuperStruct = Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingChildStructSimpleLambda") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = ConstraintHindleyMilner.both)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantApplicationFail") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq())
    val takesSuperStruct = Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingChildStructLambda") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(Field("y", IntLanguageType)), Some("s"))
    val newChild = Binding("newChild", New("s2", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val takesSuperStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), Variable("newChild"))), Some(IntLanguageType))
    val module = Module("module", Seq(newChild, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = ConstraintHindleyMilner.both)
  }

  test("lambdaTakingParentAndChildStructLambda") {
    val structParent = Struct("parent", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("child", Seq(Field("y", IntLanguageType)), Some("parent"))
    val newParent = Binding("newParent", New("parent", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("parent")))
    val newChild = Binding("newChild", New("child", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new LanguageStructType("child")))
    val takesSuperStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new LanguageStructType("parent")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Add(
        Application(Variable("takesSuperStruct"), Variable("newChild")),
        Application(Variable("takesSuperStruct"), Variable("newParent")))),
      Some(IntLanguageType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = ConstraintHindleyMilner.both)
  }

  test("genericLambdaTakingParentAndChildStruct") {
    val structParent = Struct("s", Seq(Field("x", IntLanguageType)))
    val structChild = Struct("s2", Seq(Field("y", IntLanguageType)), Some("s"))
    val newChild = Binding("newChild", New("s2", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val newParent = Binding("newParent", New("s", Seq(StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesSuperStruct = Lambda("struct", Access(Variable("struct"), "x"))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Add(Application(Variable("takesSuperStruct"), Variable("newChild")), Application(Variable("takesSuperStruct"), Variable("newParent")))), Some(IntLanguageType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program, skip = ConstraintHindleyMilner.both)
  }
}
