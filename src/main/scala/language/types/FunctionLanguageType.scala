package language.types

import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = {
    val inputType = builder.typeVariable()
    val outputType = builder.typeVariable()
    builder.typesAreEqual(_type, Language.getFunctionType(inputType, outputType))
    argument.constraints(builder, inputType, scope)
    result.constraints(builder, outputType, scope)
  }
}
