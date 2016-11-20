import org.scalatest.FunSuite

class PolymorphicTypes extends FunSuite {

  test("lambda") {
    val program = new Lambda("x", new Variable("x"))
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

  test("reuseIdentity2") {
    val identity = new Lambda("x", new Variable("x"))
    val program = new Let("identity", identity, Application(new Variable("identity"), Application(new Variable("identity"), Const(3))))
    assert(StaticChecker.check(program))
  }
}
