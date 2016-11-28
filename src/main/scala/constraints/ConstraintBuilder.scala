package constraints

import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.{DeclarationInsideScope, ParentScope, ReferenceInScope}
import constraints.scopes.objects._
import constraints.scopes.objects.ConcreteScope
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}
import constraints.types.objects.{PrimitiveType, Type, TypeApplication, TypeVariable}
import language.Program
import language.expressions.{ClosureLambda, Expression}
import language.types.LanguageType
import modes.ConstraintChecker

import scala.collection.mutable

case class Copy(key: AnyRef, counter: Int)
class ConstraintBuilder(factory: Factory, val mode: ConstraintChecker) {
  var copyCounter = 0
  def copy(id: AnyRef) : Copy = {
    copyCounter += 1
    Copy(id, copyCounter)
  }

  val typeVariables: scala.collection.mutable.Map[String, TypeVariable] = mutable.Map.empty   //TODO deze moeten nog resetten

  def scopeVariable(parent: Option[Scope] = None): ScopeVariable = {
    val result = factory.scopeVariable
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def newScope(): ConcreteScope = factory.newScope

  def typeVariable(): TypeVariable = factory.typeVariable

  var constraints: List[Constraint] = List.empty

  def getFunctionType(argument: Type, result: Type) = TypeApplication(PrimitiveType("Func"), Seq(argument, result))
  def newScope(parent: Option[Scope]) : ConcreteScope = {
    val result = factory.newScope
    parent.foreach(p => constraints ::= ParentScope(result, p))
    result
  }

  def resolve(name: String, id: AnyRef, scope: Scope) : DeclarationVariable = {
    val result = declarationVariable()
    reference(name, id, scope, result)
    result
  }

  def reference(name: String, id: AnyRef, scope: Scope, declaration: Declaration) : Reference = {
    val result = Reference(name, id)
    constraints ::= ReferenceInScope(result, scope) //TODO waarom maakt het uit als ik deze twee omdraai?
    constraints ::= ResolvesTo(result, declaration)
    result
  }

  def declarationType(name: String, id: AnyRef, container: Scope) : Type  = {
    val result = typeVariable()
    declaration(name, id, container, Some(result))
    result
  }

  def declaration(name: String, id: AnyRef, container: Scope, _type: Option[Type] = None): NamedDeclaration = {
    val result = NamedDeclaration(name, id)
    constraints ::= DeclarationInsideScope(result, container)
    _type.foreach(t => constraints ::= DeclarationOfType(result, t))
    result
  }

  def specialization(first: Type, second: Type, debugInfo: Any = null): Unit = add(Specialization(first, second, debugInfo))
  def typesAreEqual(first: Type, second: Type): Unit = add(TypesAreEqual(first, second))

  def add(addition: Constraint): Unit = constraints ++= Seq(addition)
  def add(addition: Seq[Constraint]): Unit = constraints ++= addition

  def declarationVariable(): DeclarationVariable = {
    factory.declarationVariable
  }

  def getType(declaration: Declaration) : Type = {
    val result = typeVariable()
    add(DeclarationOfType(declaration, result))
    result
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
