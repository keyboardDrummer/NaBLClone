package modes

import bindingTypeMachine.{Machine, MachineType}
import language.Program
import language.expressions.Expression
import language.types.LanguageType

object MachineChecker extends Checker
{
  override def check(program: Program): Boolean =  {
    val machine: Machine = new Machine()
    try
    {
      program.evaluate(machine)
      true
    }
    catch
    {
      case e: Throwable =>
        val f = e
        Console.out.append("")
        false
    }
  }

  override def checkExpression(expression: Expression, _type: LanguageType): Boolean = {
    val expressionMachine: Machine = Machine.expressionMachine
    try
    {
      val machineType = expression.evaluate(expressionMachine)
      val expectedMachineType: MachineType = _type.evaluate(expressionMachine)
      expectedMachineType == machineType
    }
    catch
    {
      case e: Throwable =>
        val f = e
        false
    }
  }
}
