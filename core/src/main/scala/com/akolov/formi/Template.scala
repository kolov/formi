package com.akolov.formi

import java.time.LocalDate

import com.akolov.formi.errors._
import org.log4s.getLogger
import cats.implicits._

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
sealed trait TemplateElement[+SV <: SValue] extends Element {
  self =>
  val label: String;
  val multiplicity: Multiplicity;

  def emptySingle: SV

  def empty: MValue
}

case class Field(
  override val label: String,
  desc: InputDesc,
  override val multiplicity: Multiplicity = Multiplicity.Once)
    extends TemplateElement[SingleFieldValue] {
  self =>

  override def emptySingle: SingleFieldValue = SingleFieldValue.Empty

  override def empty: MValue = emptyField

  def emptyField: MultiFieldValue = MultiFieldValue(Vector.fill(Math.max(multiplicity.minOccurs, 1))(self.emptySingle))
}

case class Group(
  override val label: String,
  fields: List[TemplateElement[SValue]],
  override val multiplicity: Multiplicity = Multiplicity.Once)
    extends TemplateElement[SingleGroupValue] {
  self =>

  override def emptySingle: SingleGroupValue = {
    val value = fields.map(v => (v.label, v.empty))
    SingleGroupValue(value)
  }

  override def empty: MValue = emptyGroup

  def emptyGroup: MultiGroupValue = {
    MultiGroupValue(Vector.fill(Math.max(multiplicity.minOccurs, 1))(self.emptySingle))
  }
}

