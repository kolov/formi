package com.akolov.formi

package compact

object CompactTemplate {
  import com.akolov.{formi => F}

  val defaultMultiplicity = Multiplicity.Once
  val defaultFieldDesc = Text()

  trait CompactTemplateElement
  case class Field(label: String, desc: Option[InputDesc]) extends CompactTemplateElement {}

  case class Group(label: String, fields: List[CompactTemplateElement], multiplicity: Option[Multiplicity])
      extends CompactTemplateElement

  def shrink(e: F.TemplateElement): CompactTemplateElement = e match {
    case F.Group(label, fields, multiplicity) =>
      Group(label, fields.map(shrink), if (multiplicity == defaultMultiplicity) None else Some(multiplicity))
    case F.Field(label, desc) => Field(label, if (desc == defaultFieldDesc) None else Some(desc))
  }

  def expand(e: CompactTemplateElement): F.TemplateElement = e match {
    case Group(label, fields, multiplicity) =>
      F.Group(label, fields.map(expand), multiplicity.getOrElse(defaultMultiplicity))
    case Field(label, desc) =>
      F.Field(label, desc.getOrElse(defaultFieldDesc))
  }
}
