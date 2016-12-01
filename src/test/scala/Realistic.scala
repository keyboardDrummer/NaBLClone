import constraints.types.objects.TypeApplication
import language.LanguageWriter
import language.structs.Struct
import language.types.{LanguageTypeApplication, LanguageTypeVariable}
import org.scalatest.FunSuite

class Realistic extends FunSuite with LanguageWriter {

  test("list with map") {
    val listDef = Struct("list", Seq("head" of "a", "tail" of LanguageTypeApplication("list", LanguageTypeVariable("a"))))
  }
}
