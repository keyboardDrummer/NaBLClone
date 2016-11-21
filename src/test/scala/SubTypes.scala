import constraints.StaticChecker
import language.Program
import language.expressions._
import language.modules.{Binding, Module}
import language.structs._
import language.types.{IntLanguageType, IntType, LongType}
import org.scalatest.FunSuite

class SubTypes extends FunSuite {

  test("intAddition") {
    val program = OverloadedAdd(Const(3), Const(2))
    assert(StaticChecker.check(program, IntType))
  }

  test("longAddition") {
    val program = OverloadedAdd(LongConst(3), LongConst(2))
    assert(StaticChecker.check(program, LongType))
  }

  test("struct") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s"))
    val structNew = new Binding("newStruct", new LanguageStructType("s2"), new New("s2", Seq(new StructFieldInit("x", Const(3)))))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("structFail") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s3"))
    val structNew = new Binding("newStruct", new LanguageStructType("s2"), new New("s2", Seq(new StructFieldInit("x", Const(3)))))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.check(program))
  }

  test("structFail2") {
    val structParent = new Struct("s", Seq(new Field("y", IntLanguageType)))
    val structChild = new Struct("s2", Seq(), Some("s"))
    val structNew = new Binding("newStruct", new LanguageStructType("s2"), new New("s2", Seq(new StructFieldInit("x", Const(3)))))
    val structUse = new Binding("structUse", IntLanguageType, new Access(new Variable("newStruct"), "x"))
    val module = new Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.check(program))
  }

  test("structBigger") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val structNew = new Binding("newStruct", new LanguageStructType("s2"), new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))))
    val structUse = new Binding("structUse", IntLanguageType, Add(new Access(new Variable("newStruct"), "x"), new Access(new Variable("newStruct"), "y")))
    val module = new Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }

  test("structBiggerFail") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val structNew = new Binding("newStruct", new LanguageStructType("s"), new New("s", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))))
    val module = new Module("module", Seq(structNew), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(!StaticChecker.check(program))
  }

  test("lambdaTakingStruct") {
    val structParent = new Struct("s", Seq(new Field("x", IntLanguageType)))
    val structChild = new Struct("s2", Seq(new Field("y", IntLanguageType)), Some("s"))
    val newChild = new Binding("newChild", new LanguageStructType("s2"), new New("s2", Seq(new StructFieldInit("x", Const(3)), new StructFieldInit("y", Const(2)))))
    val newParent = new Binding("newParent", new LanguageStructType("s"), new New("s", Seq(new StructFieldInit("x", Const(3)))))
    val takesSuperStruct = new Lambda("struct", new Access(new Variable("struct"), "x"))
    val structUse = new Binding("structUse", IntLanguageType, new Let("takesSuperStruct", takesSuperStruct,
        Add(Application(new Variable("takesSuperStruct"), new Variable("newChild")), Application(new Variable("takesSuperStruct"), new Variable("newParent")))))
    val module = new Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    assert(StaticChecker.check(program))
  }
}
