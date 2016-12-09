import language.{LanguageWriter, Program}
import language.expressions.Add
import language.modules.{Binding, Module}
import language.structs._
import language.types.{BoolType, IntType}
import modes.MachineChecker
import org.scalatest.FunSuite

class UnionTypes extends FunSuite with LanguageWriter {

  test("matchTypeZero") {
    val baseType = Struct("intOrBool", Seq.empty)
    val usage = Binding("main", MatchType(New("intOrBool", Seq()), Seq(TypeCase("intOrBool", "y", 1))))
    val module = Module("module", Seq(usage), Seq(baseType))
    Checker.check(Program(Seq(module)), skip = Set(MachineChecker))
  }

  test("matchTypeSingle") {
    val baseType = Struct("intOrBool", Seq.empty)
    val iConstructor = Struct("i", Seq(Field("x", IntType)), parent = Some("intOrBool"))
    val usage = Binding("main", MatchType(New("i", Seq("x" is 3)), Seq(TypeCase("i", "y", Add(2,Access("y","x"))))))
    val module = Module("module", Seq(usage), Seq(baseType, iConstructor))
    Checker.check(Program(Seq(module)), skip = Set(MachineChecker))
  }

  test("basicMatchType") {
    val baseType = Struct("intOrBool", Seq.empty)
    val iConstructor = Struct("i", Seq(Field("x", IntType)), parent = Some("intOrBool"))
    val bConstructor = Struct("b", Seq(Field("r", BoolType)), parent = Some("intOrBool"))
    val usage = Binding("main", MatchType(New("intOrBool", Seq.empty), Seq(TypeCase("b", "z", 1), TypeCase("i", "y", Add(2,Access("y","x"))))))
    val module = Module("module", Seq(usage), Seq(baseType, iConstructor, bConstructor))
    Checker.check(Program(Seq(module)), skip = Set(MachineChecker))
  }
}
