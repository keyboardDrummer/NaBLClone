import bindingTypeMachine.{ForAllType, FunctionType, MachineTypeVariable}
import constraints.StaticChecker
import constraints.types.objects.TypeVariable
import language._
import language.expressions._
import language.types._
import org.scalatest.FunSuite

class PolymorphicTypes extends FunSuite {

  test("boolIntoIdentity") {
    val program = Application(new Lambda("x", new Variable("x")), BoolConst(true))
    assert(!StaticChecker.both(program))
  }

  test("lambda") {
    val program = new Lambda("x", new Variable("x"))
    assert(!StaticChecker.both(program))
  }

  test("lambda2") {
    val program = new Lambda("x", new Variable("x"))
    assert(StaticChecker.check(program, TypeVariable("jo")))
  }

  test("letIdentity") {
    val program = new Let("identity", new Lambda("x", new Variable("x")), Const(3))
    assert(StaticChecker.both(program))
  }

  test("letIdentity2") {
    val program = new Let("identity", new Lambda("x", new NoSpecializeVariable("x")), Const(3))
    assert(StaticChecker.both(program))
  }

  test("identitySquareIsNoInt") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Application(identity, identity2)
    assert(!StaticChecker.both(program))
  }

  test("identitySquareIsNoInt2") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Add(Application(identity, identity2), Const(2))
    assert(!StaticChecker.both(program))
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", new Variable("x")), Const(3))
    assert(StaticChecker.both(program))
  }

  test("reuseIdentity") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), Const(3)))
    assert(StaticChecker.both(program, IntLanguageType))
  }

  test("reuseIdentityFail") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), BoolConst(true)))
    assert(!StaticChecker.both(program, IntLanguageType))
  }

  test("lambdaDoesNotGeneralize") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    assert(!StaticChecker.check(program))
  }

  test("lambdaDoesNotGeneralizeMachine") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    assert(StaticChecker.checkMachine(program))
  }

  test("referenceIdentityChangeType") {
    val identity = new Lambda("x", new NoSpecializeVariable("x"))
    val program = new Let("identity", identity, Application(new NoSpecializeVariable("identity"), BoolConst(true)))
    assert(!StaticChecker.both(program))
  }

  test("referenceIdentityChangeType2") {
    val identity = new Lambda("x", new NoSpecializeVariable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    assert(!StaticChecker.both(program))
  }

  test("referenceIdentityChangeType3") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    assert(!StaticChecker.both(program))
  }

  test("reuseIdentityWithoutSpecialization") {
    val identity = new Lambda("x", new NoSpecializeVariable("x"))
    val program = new Let("identity", identity, Application(Application(new NoSpecializeVariable("identity"), new NoSpecializeVariable("identity")), Const(3)))
    assert(!StaticChecker.check(program))
  }

  test("reuseIdentity2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Application(new Variable("identity"), Const(3))))
    assert(StaticChecker.both(program))
  }

  test("const" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, Application(Application(new Variable("const"), Const(3)), BoolConst(true)))
    assert(StaticChecker.both(program))
  }

  test("const2" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(Application(new Variable("constSquare"), Const(2)), Const(3)), Const(4))))
    assert(StaticChecker.both(program))
  }

  ignore("constFail" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(new Variable("constSquare"), Const(2)), Const(3))))
    assert(!StaticChecker.both(program))
  }

  test("poly")
  {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Const(3)),
      Some(LanguageForAllType("a", FunctionLanguageType(LanguageTypeVariable("a"), LanguageTypeVariable("a")))))
    assert(StaticChecker.checkMachine(program))
  }
}
