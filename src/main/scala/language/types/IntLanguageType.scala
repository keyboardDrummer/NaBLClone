package language.types

import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

object LongLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, LongType))
}

object IntLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

object BoolLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, BoolType))
}