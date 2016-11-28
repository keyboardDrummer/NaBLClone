package bindingTypeMachine

import language.modules.Module

case class TypeCheckException(message: String) extends RuntimeException(message)

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
    val module = Module("empty", Seq.empty, Seq.empty, Seq.empty)
    result.newModule(module)
    result.addSubType(IntMachineType, LongMachineType)
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
  def declareStruct(structType: StructMachineType): Unit = currentModule.structDefinitions += (structType.name -> structType)

  def enterScope(): Unit = currentScope = ExpressionScope(currentScope)
  def exitScope(): Unit = currentScope = currentScope.asInstanceOf[ExpressionScope].parent
  def resolveStruct(name: String): StructMachineType = currentModule.resolveStruct(name)

  val typeGraph = new TypeGraph

  def assertSubType(subType: MachineType, superType: MachineType): Unit = {
    if (!isSubType(subType, superType))
      throw new IllegalStateException()
  }

  def isSubType(subType: MachineType, superType: MachineType): Boolean = {
    if (areEqual(subType, superType)) {
      return true
    }
    typeGraph.isSuperType(superType, subType)
  }

  def areEqual(realType: MachineType, evaluatedType: MachineType): Boolean = {
    if (realType != evaluatedType) {
      (realType, evaluatedType) match {
        case (c: MachineClosureType, f: FunctionType) =>
          areFunctionAndClosureEqual(f, c)
        case (f: FunctionType, c: MachineClosureType) =>
          areFunctionAndClosureEqual(f, c)
        case _ => false
      }
    }
    else
      true
  }

  def assertEqual(realType: MachineType, evaluatedType: MachineType): Unit = {
    if (!areEqual(realType, evaluatedType))
      throw new IllegalStateException()
  }

  def areFunctionAndClosureEqual(f: FunctionType, c: MachineClosureType): Boolean = {
    val current = currentScope
    currentScope = c.scope
    enterScope()
    declare(c.name, f.input)
    val realOutput = c.getType(this)
    val result = areEqual(f.output, realOutput)
    exitScope()
    currentScope = current
    result
  }

  def declare(name: String, _type: MachineType): Unit = {
    currentScope.asInstanceOf[ExpressionScope].declare(name, _type)
  }

  def addSubType(subType: MachineType, superType: MachineType): Any = {
    typeGraph.add(subType, superType)
  }
}
