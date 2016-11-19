/**
  * Created by RemyW on 18-11-2016.
  */
class Factory
{
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
