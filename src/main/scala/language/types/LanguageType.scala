package language.types

import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

trait LanguageType
{
  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope)
}
