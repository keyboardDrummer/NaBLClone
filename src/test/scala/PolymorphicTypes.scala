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
}
