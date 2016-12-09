import language.LanguageWriter
import language.expressions.Lambda
import language.structs.Struct
import language.types.LanguageTypeApplication
import org.scalatest.FunSuite

class Realistic extends FunSuite with LanguageWriter {

  test("list with map") {
    val listDef = Struct("list", Seq("head" of "a", "tail" of LanguageTypeApplication("list", "a")))
    //val mapDef = Lambda("f", Lambda("xs"))
  }
}
