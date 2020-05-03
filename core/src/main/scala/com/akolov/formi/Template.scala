package com.akolov.formi

import java.time.LocalDate

case class Multiplicity(minOccurs: Int, maxOccurs: Option[Int] = None) {
  def isUnderMax(n: Int): Boolean = maxOccurs.map(n < _).getOrElse(true)
}

object Multiplicity {
  val Once = Multiplicity(1, Some(1))
  val AtLeastOnce = Multiplicity(1, None)
  val Optional = Multiplicity(0, Some(1))
  def apply(minOccurs: Int, maxOccurs: Int) = new Multiplicity(minOccurs, Some(maxOccurs))
}

sealed trait InputDesc
case class Text(maxLength: Option[Int] = None, pattern: Option[String] = None) extends InputDesc
case class Date(notBefore: Option[LocalDate]) extends InputDesc

sealed trait Element

/*
 * Element describing a single value of type SV
 */
sealed trait TemplateElement extends Element {
  val label: String;
  def empty: Value
}

case class Field(override val label: String, desc: InputDesc) extends TemplateElement {
  override def empty: Value = FieldValue.Empty
}

case class Group(
  override val label: String,
  fields: List[TemplateElement],
  multiplicity: Multiplicity = Multiplicity.Once)
    extends TemplateElement {
  self =>

  def singleEmpty: SingleGroupValue = SingleGroupValue(fields.map(te => (te.label, te.empty)).toMap)

  override def empty: GroupValue = {
    GroupValue(Vector.fill(Math.max(multiplicity.minOccurs, 1))(self.singleEmpty))
  }
}
