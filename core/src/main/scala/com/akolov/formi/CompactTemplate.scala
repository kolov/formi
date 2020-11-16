package com.akolov.formi

package compact

object CompactTemplate {
  import com.akolov.{formi => F}

  val defaultMultiplicity = Multiplicity.Once
  val defaultFieldDesc = InputDesc(`type` = "text")

  case class CompactTemplateElement(
    label: String,
    input: Option[InputDesc],
    fields: Option[List[CompactTemplateElement]],
    multiplicity: Option[Multiplicity])

  def expandGroup(e: CompactTemplateElement): Option[F.Group] = expand(e) match {
    case g: F.Group => Some(g)
    case _ => None
  }

  def expand(e: CompactTemplateElement): F.TemplateElement = e match {
    case CompactTemplateElement(label, input, None, None) =>
      F.Field(label, input.getOrElse(defaultFieldDesc))
    case CompactTemplateElement(label, None, Some(fields), multiplicity) =>
      F.Group(label, fields.map(expand), multiplicity.getOrElse(defaultMultiplicity))
  }
}
