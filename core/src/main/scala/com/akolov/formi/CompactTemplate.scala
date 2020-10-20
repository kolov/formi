package com.akolov.formi

package compact

import com.akolov.formi

object CompactTemplate {
  import com.akolov.{formi => F}

  val defaultMultiplicity = Multiplicity.Once
  val defaultFieldDesc = InputDesc(`type` = "text")

  sealed trait CompactTemplateElement
  case class Field(label: String, input: Option[InputDesc], desc: Option[String]) extends CompactTemplateElement {}

  case class Group(
    label: String,
    fields: List[CompactTemplateElement],
    multiplicity: Option[Multiplicity],
    desc: Option[String] = None)
      extends CompactTemplateElement {

    def expand: formi.Group =
      F.Group(label, fields.map(CompactTemplate.expand), multiplicity.getOrElse(defaultMultiplicity))
  }

  def shrink(e: F.TemplateElement): CompactTemplateElement = e match {
    case F.Group(label, fields, multiplicity, desc) =>
      Group(label, fields.map(shrink), if (multiplicity == defaultMultiplicity) None else Some(multiplicity), desc)
    case F.Field(label, input, desc) => Field(label, if (input == defaultFieldDesc) None else Some(input), desc)
  }

  def expand(e: CompactTemplateElement): F.TemplateElement = e match {
    case Group(label, fields, multiplicity, desc) =>
      F.Group(label, fields.map(expand), multiplicity.getOrElse(defaultMultiplicity), desc)
    case Field(label, input, desc) =>
      F.Field(label, input.getOrElse(defaultFieldDesc), desc)
  }
}
