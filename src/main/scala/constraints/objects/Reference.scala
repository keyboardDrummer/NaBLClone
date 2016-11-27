package constraints.objects

case class Reference(name: String, id: AnyRef)
{
  override def hashCode(): Int = name.hashCode * id.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Reference => name.equals(other.name) && id.eq(other)
    case _ => false
  }
}
