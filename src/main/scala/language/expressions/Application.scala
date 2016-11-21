package language.expressions

import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))) ++
      function.constraints(builder, functionType, scope) ++
      value.constraints(builder, argumentType, scope)
  }
}
