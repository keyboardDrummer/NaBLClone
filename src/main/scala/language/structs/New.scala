package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.objects.{Declaration, NamedDeclaration}
import constraints.scopes.objects.Scope
import constraints.types.InstantiateDeclarationConstraint
import constraints.types.objects.{StructConstraintType, Type, TypeApplication}
import language.expressions.Expression
import language.types.LanguageType

case class New(structName: String, fieldInitializers: Seq[StructFieldInit], genericTypeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(structName, this, parentScope, structDeclaration)
    val structType: Type = genericTypeArgument.fold[Type](StructConstraintType(structDeclaration))((t: LanguageType) => {
      val typeArgument = t.constraints(builder, parentScope)
      val instantiatedStruct = builder.declarationVariable()
      builder.add(InstantiateDeclarationConstraint(typeArgument, instantiatedStruct, structDeclaration))
      StructConstraintType(instantiatedStruct) //TypeApplication(StructType(instantiatedStruct), Seq(typeArgument))
    })
    val instantiatedStructDeclaration: Declaration = structType.function.asInstanceOf[StructConstraintType].declaration
    val structScope = builder.declaredScopeVariable(instantiatedStructDeclaration)
    builder.typesAreEqual(_type, structType)
    fieldInitializers.foreach(value => value.constraints(builder, structScope, parentScope))
  }

  override def evaluate(machine: Machine): MachineType = {
    val structType = machine.resolveStruct(structName)
    val instantiatedStructType = genericTypeArgument.fold(structType)(t => structType.instantiate(structType.parameterType.get, t.evaluate(machine)))

    fieldInitializers.foreach(fieldInit => {
      val fieldType = instantiatedStructType.resolve(fieldInit.fieldName)
      machine.assertSubType(fieldInit.value.evaluate(machine), fieldType)
    })
    instantiatedStructType
  }
}
