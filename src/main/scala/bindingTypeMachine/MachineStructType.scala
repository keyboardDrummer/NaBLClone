package bindingTypeMachine

case class MachineStructType(name: String, fields: Map[String, MachineType], parent: Option[MachineStructType] = None,
                             parameterType: Option[String] = None) extends MachineType
{
  def resolve(field: String): MachineType = fields.get(field).orElse(parent.map(p => p.resolve(field))).get

  override def instantiate(name: String, value: MachineType): MachineStructType =
    MachineStructType(name, fields.mapValues(v => v.instantiate(name, value)), parent.map(p => p.instantiate(name, value)),
      parameterType)

  override def variables: Set[MachineTypeVariable] = fields.values.flatMap(field => field.variables).toSet
}
