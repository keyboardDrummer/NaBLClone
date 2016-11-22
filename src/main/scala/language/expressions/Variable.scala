package language.expressions

import constraints.objects.{DeclarationVariable, Reference}
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class NoSpecializeVariable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val declarationType = builder.typeVariable()
    val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
    builder.reference(name, this, scope, declaration)
    builder.getConstraints ++ Seq(TypesAreEqual(_type, declarationType))
  }
}

class Variable2(name: String) extends Expression { //TODO why does this break the test lambdaTakingStruct ??
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val declarationType = builder.typeVariable()
    val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
    builder.reference(name, this, scope, declaration)
    builder.getConstraints ++ Seq(Specialization(_type, declarationType))
  }
}

class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val declaration: DeclarationVariable = builder.declarationVariable()
    val reference: Reference = Reference(name, this)
    val declarationType = builder.typeVariable()
    Seq(DeclarationOfType(declaration, declarationType), ReferenceInScope(reference, scope), ResolvesTo(reference, declaration),
      Specialization(_type, declarationType))
  }
}
