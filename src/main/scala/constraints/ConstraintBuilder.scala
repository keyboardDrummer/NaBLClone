package constraints

import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import constraints.scopes.objects._
import constraints.scopes.objects.ConcreteScope
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}
import constraints.types.objects.{Type, TypeVariable}

import scala.collection.mutable

trait Mode
object HindlerMilner extends Mode
object AbstractMachine extends Mode

class ConstraintBuilder(factory: Factory, val mode: Mode) {

  val typeVariables: scala.collection.mutable.Map[String, TypeVariable] = mutable.Map.empty

  def scopeVariable(parent: Option[Scope] = None): ScopeVariable = {
    val result = factory.scopeVariable
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def newScope(): ConcreteScope = factory.freshScope

  def typeVariable(): TypeVariable = factory.typeVariable

  var constraints: List[Constraint] = List.empty

  def newScope(parent: Option[Scope]) : ConcreteScope = {
    val result = factory.freshScope
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def reference(name: String, id: AnyRef, scope: Scope, declaration: Declaration) : Reference = {
    val result = Reference(name, id)
    constraints ::= ReferenceInScope(result, scope) //TODO waarom maakt het uit als ik deze twee omdraai?
    constraints ::= ResolvesTo(result, declaration)
    result
  }

  def declaration(name: String, id: AnyRef, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    val result = NamedDeclaration(name, id)
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationOfType(result, t))
    result
  }

  def specialization(first: Type, second: Type, debugInfo: Any = null): Unit = add(new Specialization(first, second, debugInfo))
  def typesAreEqual(first: Type, second: Type) = add(TypesAreEqual(first, second))

  def add(addition: Constraint) = constraints ++= Seq(addition)
  def add(addition: Seq[Constraint]) = constraints ++= addition

  def declarationVariable(): DeclarationVariable = {
    factory.declarationVariable
  }

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

  def declaredNewScope(declaration: Declaration, parent: Option[Scope] = None): ConcreteScope = {
    val result = newScope(parent)
    constraints ::= DeclarationOfScope(declaration, result)
    result
  }

  def getConstraints: Seq[Constraint] = {
    val result = constraints.reverse
    constraints = List.empty
    result
  }
}
