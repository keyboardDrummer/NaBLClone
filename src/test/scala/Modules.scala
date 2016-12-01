import language.{LanguageWriter, Program}
import language.expressions.{Const, Variable}
import language.modules.{Binding, Module, ModuleImport}
import language.types.IntType
import org.scalatest.FunSuite

class Modules extends FunSuite with LanguageWriter  {

  test("module") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntType))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.check(program)
  }

  test("moduleFail") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntType))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.fail(program)
  }
}
