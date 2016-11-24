package bindingTypeMachine

import constraints.Factory

trait VariableScope
{
  def resolve(name: String): MachineType
  def currentModule : ModuleScope
}

class ModuleScope(machine: Machine, name: String) extends VariableScope
{
  def resolveStruct(name: String): StructMachineType = structDefinitions(name)

  var structDefinitions: Map[String, StructMachineType] = Map.empty
  var imports: List[ModuleScope] = List.empty
  var bindings: Map[String, MachineType] = Map.empty

  def addBinding(name: String, _type: MachineType) = {
    bindings += name -> _type
  }

  override def resolve(name: String): MachineType = bindings(name)

  override def currentModule: ModuleScope = this
}

case class ExpressionScope(parent: VariableScope) extends VariableScope
{
  def declare(name: String, _type: MachineType): Unit = {
    environment += name -> _type
  }

  var environment: Map[String, MachineType] = Map.empty

  def resolve(name: String): MachineType = environment.getOrElse(name, parent.resolve(name))

  override def currentModule: ModuleScope = parent.currentModule
}

object Machine
{
  def expressionMachine: Machine = {
    val result = new Machine()
    result.newModule("empty")
    result.enterScope()
    result
  }
}

class Machine {
  def newModule(name: String) = {
    currentScope = new ModuleScope(this, name)
    modules += name -> currentScope.currentModule
  }

  def currentModule: ModuleScope = currentScope.currentModule
  def resolveModule(name: String): ModuleScope = modules(name)

  def resolve(name: String): MachineType = currentScope.resolve(name)

  var currentScope: VariableScope = _
  var modules: Map[String, ModuleScope] = Map.empty
  def declareStruct(structType: StructMachineType) = currentModule.structDefinitions += (structType.name -> structType)

  def enterScope() = currentScope = ExpressionScope(currentScope)
  def exitScope() = currentScope = currentScope.asInstanceOf[ExpressionScope].parent
  def resolveStruct(name: String): StructMachineType = currentModule.resolveStruct(name)

  val typeGraph = new TypeGraph

  def assertSubType(subType: MachineType, superType: MachineType) = {

  }

  def assertEqual(realType: MachineType, evaluatedType: MachineType) =
    if (realType != evaluatedType)
      throw new IllegalStateException()

  def subType(subType: StructMachineType, superType: StructMachineType) = {

  }

  def declare(name: String, _type: MachineType): Unit = {
    currentScope.asInstanceOf[ExpressionScope].declare(name, _type)
  }

  def addSubType(subType: MachineType, superType: MachineType) = {
    typeGraph.add(subType, superType)
  }
}
