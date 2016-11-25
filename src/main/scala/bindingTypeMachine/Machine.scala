package bindingTypeMachine

import language.modules.Module

trait VariableScope
{
  def resolve(name: String): MachineType
  def currentModule : ModuleScope
}

class ModuleScope(machine: Machine, module: Module) extends VariableScope
{
  private var initialized: Boolean = false
  def initialize(): Unit = {
    if (!initialized)
    {
      initialized = true
      machine.currentScope = this
      module.imports.foreach(_import => _import.evaluate(machine))
      module.structs.foreach(struct => struct.evaluate(machine))
      module.bindings.foreach(binding => binding.evaluate(machine))
    }
  }

  def resolveStruct(name: String): StructMachineType = structDefinitions(name)

  var structDefinitions: Map[String, StructMachineType] = Map.empty
  var imports: List[ModuleScope] = List.empty
  var bindings: Map[String, MachineType] = Map.empty

  def addBinding(name: String, _type: MachineType): Unit = {
    bindings += name -> _type
  }

  override def resolve(name: String): MachineType = bindings.getOrElse(name, imports.map(i => i.resolve(name)).head)

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
    val module = new Module("empty",Seq.empty, Seq.empty, Seq.empty)
    result.newModule(module)
    result.enterScope()
    result
  }
}

class Machine {
  def newModule(module: Module): Unit = {
    currentScope = new ModuleScope(this, module)
    modules += module.name -> currentScope.currentModule
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

  def assertEqual(realType: MachineType, evaluatedType: MachineType): Unit =
    if (realType != evaluatedType)
    {
      (realType, evaluatedType) match {
        case (c: ClosureType, f: FunctionType) =>
          assertFunctionAndClosureAreEqual(f, c)
        case (f: FunctionType, c: ClosureType) =>
          assertFunctionAndClosureAreEqual(f, c)
        case _ => throw new IllegalStateException()
      }
    }

  def assertFunctionAndClosureAreEqual(f: FunctionType, c: ClosureType): Unit = {
    val current = currentScope
    currentScope = c.scope
    declare(c.name, f.input)
    val realOutput = c.getType(this)
    assertEqual(f.output, realOutput)
    currentScope = current
  }

  def subType(subType: StructMachineType, superType: StructMachineType) = {

  }

  def declare(name: String, _type: MachineType): Unit = {
    currentScope.asInstanceOf[ExpressionScope].declare(name, _type)
  }

  def addSubType(subType: MachineType, superType: MachineType) = {
    typeGraph.add(subType, superType)
  }
}
