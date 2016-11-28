
import language.expressions._
import language.types._
import modes.{ConstraintClosure, ConstraintHindleyMilner, MachineChecker}
import org.scalatest.FunSuite

class PolymorphicTypes extends FunSuite {

  test("boolIntoIdentity") {
    val program = Application(new Lambda("x", new Variable("x")), BoolConst(true))
    Checker.failExpression(program)
  }

  test("lambda") {
    val program = new Lambda("x", new Variable("x"))
    Checker.failExpression(program)
  }

  test("lambda2") {
    val program = new Lambda("x", new Variable("x"))
    Checker.checkExpression(program, LanguageTypeVariable("jo"), modes = Set(ConstraintHindleyMilner, ConstraintClosure))
  }

  test("letIdentity") {
    val program = new Let("identity", new Lambda("x", new Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("letIdentity2") {
    val program = new Let("identity", new Lambda("x", new Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("identitySquareIsNoInt") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Application(identity, identity2)
    Checker.failExpression(program)
  }

  test("identitySquareIsNoInt2") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Add(Application(identity, identity2), Const(2))
    Checker.failExpression(program)
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", new Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("reuseIdentity") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), Const(3)))
    Checker.checkExpression(program, IntLanguageType, modes = Set(MachineChecker, ConstraintHindleyMilner))
  }

  test("reuseIdentityFail") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), BoolConst(true)))
    Checker.failExpression(program, IntLanguageType)
  }

  test("lambdaDoesNotGeneralize") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    Checker.checkExpression(program, modes = Set(MachineChecker))
  }

  test("lambdaDoesNotGeneralizeMachine") {
    val identity = new Lambda("x", new Variable("x"))
    val program = Application(new Lambda("identity", Application(Application(new Variable("identity"), new Variable("identity")), Const(3))), identity)
    Checker.checkExpression(program, modes = Set(MachineChecker))
  }

  test("referenceIdentityChangeType") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("referenceIdentityChangeType2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("referenceIdentityChangeType3") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("reuseIdentity2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Application(new Variable("identity"), Const(3))))
    Checker.checkExpression(program)
  }

  test("const" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, Application(Application(new Variable("const"), Const(3)), BoolConst(true)))
    Checker.checkExpression(program)
  }

  test("const2" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(Application(new Variable("constSquare"), Const(2)), Const(3)), Const(4))))
    Checker.checkExpression(program, modes = Set(MachineChecker, ConstraintHindleyMilner))
  }

  ignore("constFail" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(Application(new Variable("constSquare"), Const(2)), Const(3))))
    Checker.failExpression(program)
  }

  test("poly")
  {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Const(3)),
      Some(LanguageForAllType("a", FunctionLanguageType(LanguageTypeVariable("a"), LanguageTypeVariable("a")))))
    Checker.checkExpression(program, modes = Set(MachineChecker, ConstraintClosure))
  }
}
