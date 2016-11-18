import org.scalatest.FunSuite


class Tests extends FunSuite {

  test("constant") {
    val program = Const(3)
    assert(StaticChecker.check(program))
  }

  test("add") {
    val program = Add(Const(3), Const(2))
    assert(StaticChecker.check(program))
  }

  test("floatingVariable") {
    val program = Variable("jo")
    assert(!StaticChecker.check(program))
  }
}
