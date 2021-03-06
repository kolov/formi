package com.akolov.formi

import java.time.LocalDate

case class Multiplicity(minOccurs: Int, maxOccurs: Option[Int] = None) {
  def allows(n: Int): Boolean = n >= minOccurs && maxOccurs.map(_ > n).getOrElse(true)

  def isUnderMax(n: Int): Boolean = maxOccurs.map(n < _).getOrElse(true)
}

object Multiplicity {
  val Once = Multiplicity(1, Some(1))
  val AtLeastOnce = Multiplicity(1, None)
  def atLeast(n: Int) = Multiplicity(n, None)
  val Optional = Multiplicity(0, Some(1))
  def apply(minOccurs: Int, maxOccurs: Int) = new Multiplicity(minOccurs, Some(maxOccurs))
}

case class InputDesc(
  `type`: String,
  multiline: Option[Boolean] = None,
  maxLength: Option[Int] = None,
  pattern: Option[String] = None,
  lines: Option[Int] = None,
  notBefore: Option[LocalDate] = None,
  inputLength: Option[Int] = None,
  inline: Option[Boolean] = None
)

sealed trait Element

/*
 * Element describing a single value of type SV
 */
sealed trait TemplateElement extends Element {
  val label: String;
  def empty: Value
}

case class Field(override val label: String, input: InputDesc) extends TemplateElement {
  override def empty: Value = FieldValue.Empty
}

case class Group(override val label: String, fields: List[TemplateElement], multiplicity: Multiplicity)
    extends TemplateElement {
  self =>

  val defaultMultiplicity = Multiplicity.Once

  def singleEmpty: SingleGroupValue = SingleGroupValue(fields.map(te => (te.label, te.empty)).toMap)

  override def empty: GroupValue = {
    GroupValue(Vector.fill(Math.max(multiplicity.minOccurs, 1))(self.singleEmpty))
  }
}

object Group {

  def apply(label: String, fields: List[TemplateElement]): Group =
    Group(label, fields, Multiplicity.Once)
}
