package constraints

import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import constraints.scopes.objects._
import constraints.scopes.objects.ConcreteScope
import constraints.types.DeclarationOfType
import constraints.types.objects.Type

class ConstraintBuilder(factory: Factory) {
  var constraints: List[Constraint] = List.empty

  def newScope(parent: Option[Scope]) : ConcreteScope = {
    val result = factory.freshScope
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def reference(name: String, id: AnyRef, scope: Scope, declaration: Declaration) : Reference = {
    val result = Reference(name, id)
    constraints ::= ResolvesTo(result, declaration)
    constraints ::= ReferenceInScope(result, scope)
    result
  }

  def declaration(name: String, id: AnyRef, _type: Type, container: Scope): NamedDeclaration = {
    val result = new NamedDeclaration(name, id)
    constraints ::= DeclarationInsideScope(result, container)
    constraints ::= DeclarationOfType(result, _type)
    result
  }

  def declarationVariable(_type: Type): DeclarationVariable = {
    val result = factory.declarationVariable
    constraints ::= DeclarationOfType(result, _type)
    result
  }

  def declaredScope(declaration: Declaration, parent: Option[Scope]): ConcreteScope = {
    val result = newScope(parent)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def getConstraints: Seq[Constraint] = {
    val result = constraints.reverse.toSeq
    constraints = List.empty
    result
  }
}
