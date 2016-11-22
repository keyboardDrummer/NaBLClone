package language.expressions

import constraints.scopes.objects.Scope
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language

case class ContraVariantApplication(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    val parameterType = builder.typeVariable()
    Seq(TypesAreEqual(functionType, Language.getFunctionType(parameterType, _type)), CheckSubType(argumentType, parameterType)) ++
      function.constraints(builder, functionType, scope) ++
      value.constraints(builder, argumentType, scope)
  }
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    function.constraints(builder, functionType, scope) ++ Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))) ++
      value.constraints(builder, argumentType, scope)
  }
}
