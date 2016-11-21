package constraints

import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import constraints.scopes.objects._
import constraints.scopes.objects.ConcreteScope
import constraints.types.DeclarationOfType
import constraints.types.objects.Type

class ConstraintBuilder(factory: Factory) {
  def scopeVariable(parent: Option[Scope] = None) = {
    val result = factory.scopeVariable
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def newScope() = factory.freshScope

  def typeVariable() = factory.typeVariable

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

  def declaration(name: String, id: AnyRef, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    val result = NamedDeclaration(name, id)
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationOfType(result, t))
    result
  }

  def declarationVariable(): DeclarationVariable = {
    factory.declarationVariable
  }

  def add(addition: Seq[Constraint]) = constraints ++= addition

  def declarationVariable(_type: Type): DeclarationVariable = {
    val result = factory.declarationVariable
    constraints ::= DeclarationOfType(result, _type)
    result
  }

  def declaredScopeVariable(declaration: Declaration, parent: Option[Scope] = None): ScopeVariable = {
    val result = scopeVariable(parent)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def getConstraints: Seq[Constraint] = {
    val result = constraints.reverse.toSeq
    constraints = List.empty
    result
  }
}
