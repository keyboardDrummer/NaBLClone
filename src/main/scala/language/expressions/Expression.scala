package language.expressions

import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

trait Expression {
  def constraints(builder: ConstraintBuilder, _type: Type, parentSocpe: Scope): Seq[Constraint]
}
