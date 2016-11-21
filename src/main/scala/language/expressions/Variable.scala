package language.expressions

import constraints.objects.{DeclarationVariable, Reference}
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import constraints.types.{DeclarationOfType, Specialization}
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val declaration: DeclarationVariable = builder.declarationVariable()
    val reference: Reference = Reference(name, this)
    val declarationType = builder.typeVariable()
    Seq(DeclarationOfType(declaration, declarationType), ReferenceInScope(reference, scope), ResolvesTo(reference, declaration),
      Specialization(_type, declarationType))
  }
}
