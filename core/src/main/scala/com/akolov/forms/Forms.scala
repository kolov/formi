package com.akolov.forms

import java.time.LocalDate

import com.akolov.forms.errors._
import org.log4s.getLogger
import cats.implicits._

// Definition
case class Multiplicity(minOccurs: Int, maxOccurs: Option[Int]) {
  def isUnderMax(n: Int): Boolean = maxOccurs.map(n < _).getOrElse(true)
}

object Multiplicity {
  val Once = Multiplicity(1, Some(1))
  val AtLeastOnce = Multiplicity(1, None)
  val Optional = Multiplicity(0, Some(1))
}

sealed trait Input

case class Text(maxLength: Option[Int] = None, pattern: Option[String] = None) extends Input

case class Date(notBefore: Option[LocalDate]) extends Input

sealed trait ElementValue

sealed trait SingleElementValue extends ElementValue

sealed trait AnyMultiValue extends ElementValue

sealed abstract class MultiElementValue[S <: SingleElementValue](val values: List[S]) extends AnyMultiValue

case class MultiFieldValue(override val values: List[SingleFieldValue])
    extends MultiElementValue[SingleFieldValue](values)

case class MultiGroupValue(override val values: List[SingleGroupValue])
    extends MultiElementValue[SingleGroupValue](values)

object MultiElementValue {
  def apply[S <: SingleElementValue](l: List[S]): MultiElementValue[S] = new MultiElementValue[S](values = l) {}
}

case class SingleFieldValue(value: Option[String]) extends SingleElementValue {

  override def toString: String =
    value.getOrElse("None")
}

case class SingleGroupValue(values: List[(String, AnyMultiValue)]) extends SingleElementValue {
  val logger = getLogger

  def get(name: String): Either[DocumentError, AnyMultiValue] = {
    values.find(_._1 === name) match {
      case None => PathError().asLeft
      case Some((_, v)) => v.asRight
    }
  }

  def update(name: String, newValue: AnyMultiValue): Either[DocumentError, SingleGroupValue] = {
    values.foldLeft((false, List[(String, AnyMultiValue)]())) {
      case ((updated, acc), el @ (k, v)) =>
        if (k === name) {
          (true, acc :+ (name, newValue))
        } else
          (updated, acc :+ el)
    } match {
      case (false, _) => PathError(s"Update failed: no such field $name").asLeft
      case (true, l) => SingleGroupValue(l).asRight
    }
  }
}

sealed trait AnyElement

sealed trait TemplateElement[+SV <: SingleElementValue] extends AnyElement {
  self =>
  val label: String;
  val multiplicity: Multiplicity;

  def emptySingle: SV

  def empty: AnyMultiValue
}

case class FieldElement(
  override val label: String,
  desc: Input,
  override val multiplicity: Multiplicity = Multiplicity.Once)
    extends TemplateElement[SingleFieldValue] {
  self =>

  override def emptySingle: SingleFieldValue = SingleFieldValue.Empty

  override def empty: AnyMultiValue = emptyField

  def emptyField: MultiFieldValue = MultiFieldValue(List(self.emptySingle))
}

case class GroupElement(
  override val label: String,
  fields: List[TemplateElement[SingleElementValue]],
  override val multiplicity: Multiplicity = Multiplicity.Once)
    extends TemplateElement[SingleGroupValue] {
  self =>

  override def emptySingle: SingleGroupValue = {
    val value = fields.map(v => (v.label, v.empty))
    SingleGroupValue(value)
  }

  override def empty: AnyMultiValue = emptyGroup

  def emptyGroup: MultiGroupValue = MultiGroupValue(List(self.emptySingle))
}

object SingleFieldValue {
  val Empty = SingleFieldValue(None)

  def apply(s: String): SingleFieldValue = new SingleFieldValue(Some(s))
}

case class Template(name: String, body: GroupElement) {
  def empty = body.emptySingle
}
