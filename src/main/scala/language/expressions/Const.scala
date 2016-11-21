package language.expressions

import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.types.{IntType, LongType}

case class LongConst(value: Long) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, LongType))
}

case class Const(value: Int) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}
