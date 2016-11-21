package language.expressions

import constraints.scopes.objects.Scope
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.types.{IntType, LongType}

case class OverloadedAdd(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(CheckSubType(_type, LongType)) ++
    left.constraints(builder, _type, scope) ++
    right.constraints(builder, _type, scope)
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType)) ++
    left.constraints(builder, IntType, scope) ++
    right.constraints(builder, IntType, scope)
}
