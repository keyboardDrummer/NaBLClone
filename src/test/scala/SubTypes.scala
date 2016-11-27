import constraints.StaticChecker
import language.Program
import language.expressions._
import language.modules.{Binding, Module}
import language.structs._
import language.types.{IntLanguageType, LongLanguageType}
import org.scalatest.FunSuite

class SubTypes extends FunSuite {

  test("intAddition") {
    val program = OverloadedAdd(Const(3), Const(2))
    assert(StaticChecker.both(program, IntLanguageType))
  }

  test("longAddition") {
    val program = OverloadedAdd(LongConst(3), LongConst(2))
    assert(StaticChecker.both(program, LongLanguageType))
  }

  test("struct") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s"))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("structFail") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s3"))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("structFail2") {
    val structParent = new Struct("s", Seq(new Field("y", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s"))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s2")))
    val structUse = new Binding("structUse", new Access(new Variable("newStruct"), "x"), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.check(program))
  }

  test("structBigger") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val structNew = new Binding("newStruct", new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val structUse = new Binding("structUse", Add(new Access(new Variable("newStruct"), "x"), new Access(new Variable("newStruct"), "y")), Some(IntLanguageType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("structBiggerFail") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val structNew = new Binding("newStruct", new New("s", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s")))
    val module = Module("module", Seq(structNew), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  test("longLambdaTakesInt") {
    val program = Application(new ContraVariantLambda("x", new Variable("x"), Some(LongLanguageType)), Const(3))
    assert(StaticChecker.both(program, IntLanguageType))
  }

  test("longLambdaTakesIntFail") {
    val program = Application(new ContraVariantLambda("x", new Variable("x"), Some(LongLanguageType)), Const(3))
    assert(!StaticChecker.both(program, LongLanguageType))
  }

  test("intLambdaTakesLong") {
    val program = Application(new ContraVariantLambda("x", new Variable("x"), Some(IntLanguageType)), LongConst(3))
    assert(!StaticChecker.both(program))
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantLambda") {
    val structParent = new Struct("s", Seq())
    val structChild = new Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = new ContraVariantLambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new NoGeneralizeLet("takesSuperStruct", takesSuperStruct,
          Application(new NoSpecializeVariable("takesSuperStruct"), new New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantLambdaFail") {
    val structParent = new Struct("s", Seq())
    val structChild = new Struct("s2", Seq())
    val takesSuperStruct = new ContraVariantLambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new NoGeneralizeLet("takesSuperStruct", takesSuperStruct,
          Application(new NoSpecializeVariable("takesSuperStruct"), new New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  ignore("lambdaTakingChildStructSimpleContraVariantLambda") {
    val structParent = new Struct("s", Seq())
    val structChild = new Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = new ContraVariantLambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Let("takesSuperStruct", takesSuperStruct,
          Application(new Variable("takesSuperStruct"), new New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantApplication") {
    val structParent = new Struct("s", Seq())
    val structChild = new Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = new Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new NoGeneralizeLet("takesSuperStruct", takesSuperStruct,
          ContraVariantApplication(new NoSpecializeVariable("takesSuperStruct"), new New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantApplicationFail") {
    val structParent = new Struct("s", Seq())
    val structChild = new Struct("s2", Seq())
    val takesSuperStruct = new Lambda("struct", Const(3), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new NoGeneralizeLet("takesSuperStruct", takesSuperStruct,
          ContraVariantApplication(new NoSpecializeVariable("takesSuperStruct"), new New("s2", Seq.empty))), Some(IntLanguageType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.both(program))
  }

  ignore("lambdaTakingChildStructContraVariantLambda") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val newChild = new Binding("newChild", new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val takesSuperStruct = new ContraVariantLambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Let("takesSuperStruct", takesSuperStruct,
          Application(new Variable("takesSuperStruct"), new Variable("newChild"))), Some(IntLanguageType))
    val module = Module("module", Seq(newChild, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  ignore("lambdaTakingParentAndChildStructContraVariantLambda") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val newChild = new Binding("newChild", new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val newParent = new Binding("newParent", new New("s", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesSuperStruct = new ContraVariantLambda("struct", new Access(new Variable("struct"), "x"), Some(new LanguageStructType("s")))
    val structUse = new Binding("structUse", new Let("takesSuperStruct", takesSuperStruct,
            Add(Application(new Variable("takesSuperStruct"), new Variable("newChild")), Application(new Variable("takesSuperStruct"), new Variable("newParent")))), Some(IntLanguageType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }

  ignore("genericLambdaTakingParentAndChildStruct") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val newChild = new Binding("newChild", new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))), Some(new LanguageStructType("s2")))
    val newParent = new Binding("newParent", new New("s", Seq(new StructFieldInit("x", Const(3)))), Some(new LanguageStructType("s")))
    val takesSuperStruct = new Lambda("struct", new Access(new Variable("struct"), "x"))
    val structUse = new Binding("structUse", new Let("takesSuperStruct", takesSuperStruct,
          Add(Application(new Variable("takesSuperStruct"), new Variable("newChild")), Application(new Variable("takesSuperStruct"), new Variable("newParent")))), Some(IntLanguageType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.both(program))
  }
}
