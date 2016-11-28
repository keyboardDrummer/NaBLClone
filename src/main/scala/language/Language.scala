package language

import constraints.types.objects.{PrimitiveType, Type, TypeApplication}
import language.expressions.Expression
import language.structs.StructFieldInit

object Language {
  def getFunctionType(argument: Type, result: Type) = TypeApplication(PrimitiveType("Func"), Seq(argument, result))
}






























