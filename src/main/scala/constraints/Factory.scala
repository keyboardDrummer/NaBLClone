package constraints

import constraints.objects.DeclarationVariable
import constraints.scopes.objects.{ConcreteScope, ScopeVariable}
import constraints.types.objects.TypeVariable

/**
  * Created by RemyW on 18-11-2016.
  */
class Factory
{
  private var scopeVariableCounter: Int = 0
  def scopeVariable: ScopeVariable = {
    scopeVariableCounter += 1
    ScopeVariable(scopeVariableCounter.toString)
  }

  private var scopeCounter: Int = 0
  def freshScope: ConcreteScope = {
    scopeCounter += 1
    ConcreteScope(scopeCounter)
  }

  private var typeCounter = 0
  def typeVariable : TypeVariable = {
    typeCounter += 1
    TypeVariable(typeCounter.toString)
  }

  private var declarationCounter = 0
  def declarationVariable: DeclarationVariable = {
    declarationCounter += 1
    DeclarationVariable(declarationCounter.toString)
  }
}
