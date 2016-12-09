import language.{LanguageWriter, Program}
import language.expressions.Add
import language.modules.{Binding, Module}
import language.types.{BoolType, IntType}
import language.unionTypes.{Case, Match, UnionOption, UnionType}
import modes.MachineChecker
import org.scalatest.FunSuite

class UnionTypes extends FunSuite with LanguageWriter {

  test("basic") {
    val usage = Binding("main", Match("i" $ 3, Seq(Case("b","x",1), Case("i", "x", Add(2,"x")))))
    val module = Module("module", Seq(usage), Seq(UnionType("intOrBool", Seq(UnionOption("i", IntType), UnionOption("b", BoolType)))))
    Checker.check(Program(Seq(module)), skip = Set(MachineChecker))
  }

  test("basicFailBoolConstructorDoesNotContainInt") {
    val usage = Binding("main", Match("i" $ 3, Seq(Case("b","x","x"), Case("i", "x", Add(2,"x")))))
    val module = Module("module", Seq(usage), Seq(UnionType("intOrBool", Seq(UnionOption("i", IntType), UnionOption("b", BoolType)))))
    Checker.fail(Program(Seq(module)))
  }

  test("basicFailPassBooleanToIntegerConstructor") {
    val usage = Binding("main", Match("i" $ true, Seq(Case("b","x",1), Case("i", "x", Add(2,"x")))))
    val module = Module("module", Seq(usage), Seq(UnionType("intOrBool", Seq(UnionOption("i", IntType), UnionOption("b", BoolType)))))
    Checker.fail(Program(Seq(module)))
  }
}
