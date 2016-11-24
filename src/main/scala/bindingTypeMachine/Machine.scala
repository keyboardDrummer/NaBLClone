package bindingTypeMachine

import constraints.Factory
import constraints.objects.{NamedDeclaration, Reference}
import constraints.scopes._
import constraints.scopes.objects.ConcreteScope
import constraints.types.TypeGraph
import constraints.types.objects.{ConcreteType, StructType, Type}
import language.types.{LanguageType, LongType}

class Machine {
  def assertSubType(subType: ConcreteType, superType: ConcreteType) = ???

  def assertEqual(realType: ConcreteType, evaluatedType: ConcreteType) = ???

  def subType(subType: StructType, superType: StructType) = ???

  def importScope(importedModule: NamedDeclaration) = {
    val importedScope: ConcreteScope = scopeGraph.resolveScope(importedModule)
    scopeGraph.addImport(currentScope, importedScope)
  }

  val factory = new Factory()
  var currentScope: ConcreteScope = _

  def newScope(): ConcreteScope = {
    currentScope = factory.freshScope
    currentScope
  }

  def nestScope() = {
    val parentScope = currentScope
    currentScope = factory.freshScope
    scopeGraph.parent(currentScope, parentScope)
  }

  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment: Map[NamedDeclaration, ConcreteType] = Map.empty

  def declareNestedScope(name: String, id: AnyRef) = {
    val declaration = declare(name, id)
    nestScope()
    scopeGraph.declareScope(declaration, currentScope)
  }

  def declare(name: String, id: AnyRef): NamedDeclaration = {
    val declaration = NamedDeclaration(name, id)
    scopeGraph.declareDeclaration(currentScope, declaration)
    declaration
  }

  def declare(name: String, id: AnyRef, _type: ConcreteType) = {
    val declaration = declare(name, id)
    environment += declaration -> _type
  }

  def resolveScope(declaration: NamedDeclaration): ConcreteScope = scopeGraph.resolveScope(declaration)

  def resolve(name: String, id: AnyRef): NamedDeclaration = {
    val reference = Reference(name, id)
    scopeGraph.addReference(reference, currentScope)
    scopeGraph.resolve(reference)
  }

  def addSubType(subType: ConcreteType, superType: ConcreteType) = {
    typeGraph.add(subType, superType)
  }
}
