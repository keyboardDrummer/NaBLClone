import constraints.StaticChecker
import language.expressions.{Const, LongConst, OverloadedAdd}
import language.types.{IntType, LongType}
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
}
