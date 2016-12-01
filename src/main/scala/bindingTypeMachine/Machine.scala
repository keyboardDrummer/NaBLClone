package bindingTypeMachine

import constraints.types.objects.TypeVariable
import language.modules.Module

case class TypeCheckException(message: String) extends RuntimeException(message)




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
  def enterTypeScope(): Unit = currentScope = TypeDefinitionScope(currentScope)

  def declareType(name: String, _type: MachineType): Unit = currentScope.asInstanceOf[TypeDefinitionScope].declareType(name, _type)

  def newModule(module: Module): Unit = {
    currentScope = ModuleScope(this, module)
    modules += module.name -> currentScope.currentModule
  }
  def resolveType(name: String): MachineType = currentScope.resolveType(name)

  def currentModule: ModuleScope = currentScope.currentModule
  def resolveModule(name: String): ModuleScope = modules(name)

  def resolve(name: String): MachineType = currentScope.resolve(name)

  var currentScope: VariableScope = _
  var modules: Map[String, ModuleScope] = Map.empty
  def declareStruct(structType: StructMachineType): Unit = currentModule.structDefinitions += (structType.name -> structType)

  def enterScope(): Unit = currentScope = ExpressionScope(currentScope)
  def exitScope(): Unit = currentScope = currentScope.parent
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

  def areEqual(realType2: MachineType, evaluatedType2: MachineType): Boolean = {
    val realType = dereferenceType(realType2)
    val evaluatedType = dereferenceType(evaluatedType2)
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
    if (!areEqual(dereferenceType(realType), dereferenceType(evaluatedType)))
      throw new IllegalStateException()
  }

  def dereferenceType(_type: MachineType): MachineType = _type match {
    case r:MachineTypeReference => r.reference
    case _ => _type
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
