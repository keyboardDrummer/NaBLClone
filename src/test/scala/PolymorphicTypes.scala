
import language.expressions._
import language.types._
import modes.{ConstraintClosure, ConstraintHindleyMilner, MachineMode}
import org.scalatest.FunSuite

class PolymorphicTypes extends FunSuite {

  test("boolIntoIdentity") {
    val program = Application(new Lambda("x", new Variable("x")), BoolConst(true))
    StaticChecker.failExpression(program)
  }

  test("lambda") {
    val program = new Lambda("x", new Variable("x"))
    StaticChecker.failExpression(program)
  }

  test("lambda2") {
    val program = new Lambda("x", new Variable("x"))
    StaticChecker.checkExpression(program, LanguageTypeVariable("jo"), modes = Set(ConstraintHindleyMilner, ConstraintClosure))
  }

  test("letIdentity") {
    val program = new Let("identity", new Lambda("x", new Variable("x")), Const(3))
    StaticChecker.checkExpression(program)
  }

  test("letIdentity2") {
    val program = new Let("identity", new Lambda("x", new Variable("x")), Const(3))
    StaticChecker.checkExpression(program)
  }

  test("identitySquareIsNoInt") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Application(identity, identity2)
    StaticChecker.failExpression(program)
  }

  test("identitySquareIsNoInt2") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Add(Application(identity, identity2), Const(2))
    StaticChecker.failExpression(program)
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", new Variable("x")), Const(3))
    StaticChecker.checkExpression(program)
  }

  test("reuseIdentity") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), Const(3)))
    StaticChecker.checkExpression(program, IntLanguageType, modes = Set(MachineMode, ConstraintHindleyMilner))
  }

  test("reuseIdentityFail") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), BoolConst(true)))
    StaticChecker.failExpression(program, IntLanguageType)
  }

  test("lambdaDoesNotGeneralize") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    StaticChecker.checkExpression(program, modes = Set(MachineMode))
  }

  test("lambdaDoesNotGeneralizeMachine") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    StaticChecker.checkMachine(program)
  }

  test("referenceIdentityChangeType") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    StaticChecker.failExpression(program)
  }

  test("referenceIdentityChangeType2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    StaticChecker.failExpression(program)
  }

  test("referenceIdentityChangeType3") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    StaticChecker.failExpression(program)
  }

  test("reuseIdentity2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Application(new Variable("identity"), Const(3))))
    StaticChecker.checkExpression(program)
  }

  test("const" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, Application(Application(new Variable("const"), Const(3)), BoolConst(true)))
    StaticChecker.checkExpression(program)
  }

  test("const2" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(Application(new Variable("constSquare"), Const(2)), Const(3)), Const(4))))
    StaticChecker.checkExpression(program, modes = Set(MachineMode, ConstraintHindleyMilner))
  }

  ignore("constFail" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(new Variable("constSquare"), Const(2)), Const(3))))
    StaticChecker.failExpression(program)
  }

  test("poly")
  {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Const(3)),
      Some(LanguageForAllType("a", FunctionLanguageType(LanguageTypeVariable("a"), LanguageTypeVariable("a")))))
    StaticChecker.checkExpression(program, modes = Set(MachineMode, ConstraintClosure))
  }
}
