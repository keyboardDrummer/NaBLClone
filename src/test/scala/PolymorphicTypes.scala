import constraints.StaticChecker
import language._
import language.expressions._
import org.scalatest.FunSuite

class PolymorphicTypes extends FunSuite {

  test("lambda") {
    val program = new Lambda("x", new Variable("x"))
    assert(!StaticChecker.check(program))
  }

  test("identitySquareIsNoInt") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Application(identity, identity2)
    assert(!StaticChecker.check(program))
  }

  test("identitySquareIsNoInt2") {
    val identity = new Lambda("x", new Variable("x"))
    val identity2 = new Lambda("x", new Variable("x"))
    val program = Add(Application(identity, identity2), Const(2))
    assert(!StaticChecker.check(program))
  }

  test("lambdaApplication") {
    val program = Application(new Lambda("x", new Variable("x")), Const(3))
    assert(StaticChecker.check(program))
  }

  test("reuseIdentity") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new Variable("identity"), new Variable("identity")), Const(3)))
    assert(StaticChecker.check(program))
  }

  test("reuseIdentityWithoutSpecialization") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(Application(new NoSpecializeVariable("identity"), new NoSpecializeVariable("identity")), Const(3)))
    assert(!StaticChecker.check(program))
  }

  test("reuseIdentity2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Application(new Variable("identity"), Const(3))))
    assert(StaticChecker.check(program))
  }

  test("const" ) {
    val const = new Lambda("x" , new Lambda("y", new Variable("x")))
    val program = new Let("const", const, new Let("constSquare", Application(new Variable("const"), new Variable("const")),
      Application(new Variable("constSquare"), Application(new Variable("const"), Const(3)))))
    assert(StaticChecker.check(program))
  }
}
