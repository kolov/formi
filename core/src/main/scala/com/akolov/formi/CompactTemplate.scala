package com.akolov.formi

package compact

import com.akolov.formi

object CompactTemplate {
  import com.akolov.{formi => F}

  val defaultMultiplicity = Multiplicity.Once
  val defaultFieldDesc = Text()

  sealed trait CompactTemplateElement
  case class Field(label: String, input: Option[InputDesc]) extends CompactTemplateElement {}

  case class Group(label: String, fields: List[CompactTemplateElement], multiplicity: Option[Multiplicity])
      extends CompactTemplateElement {

    def expand: formi.Group =
      F.Group(label, fields.map(CompactTemplate.expand), multiplicity.getOrElse(defaultMultiplicity))
  }

  def shrink(e: F.TemplateElement): CompactTemplateElement = e match {
    case F.Group(label, fields, multiplicity) =>
      Group(label, fields.map(shrink), if (multiplicity == defaultMultiplicity) None else Some(multiplicity))
    case F.Field(label, input) => Field(label, if (input == defaultFieldDesc) None else Some(input))
  }

  def expand(e: CompactTemplateElement): F.TemplateElement = e match {
    case Group(label, fields, multiplicity) =>
      F.Group(label, fields.map(expand), multiplicity.getOrElse(defaultMultiplicity))
    case Field(label, input) =>
      F.Field(label, input.getOrElse(defaultFieldDesc))
  }
}
