package com.akolov.formi

import java.time.LocalDate

import com.akolov.formi.errors._
import org.log4s.getLogger
import cats.implicits._

sealed trait InputValue
sealed trait SValue extends InputValue
sealed trait MValue extends InputValue

sealed abstract class MultiElementValue[S <: SValue](val values: Seq[S]) extends MValue

case class MultiFieldValue(override val values: Seq[SingleFieldValue])
    extends MultiElementValue[SingleFieldValue](values)

case class MultiGroupValue(override val values: Seq[SingleGroupValue])
    extends MultiElementValue[SingleGroupValue](values)

object MultiElementValue {
  def apply[S <: SValue](l: Seq[S]): MultiElementValue[S] = new MultiElementValue[S](values = l) {}
}

case class SingleFieldValue(value: Option[String]) extends SValue

case class SingleGroupValue(values: Seq[(String, MValue)]) extends SValue {
  val logger = getLogger

  def get(name: String): Either[DocumentError, MValue] = {
    values.find(_._1 === name) match {
      case None => PathError().asLeft
      case Some((_, v)) => v.asRight
    }
  }

  def update(name: String, newValue: MValue): Either[DocumentError, SingleGroupValue] = {
    values.foldLeft((false, List[(String, MValue)]())) {
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

case class Multiplicity(minOccurs: Int, maxOccurs: Option[Int] = None) {
  def isUnderMax(n: Int): Boolean = maxOccurs.map(n < _).getOrElse(true)
}

object Multiplicity {
  val Once = Multiplicity(1, Some(1))
  val AtLeastOnce = Multiplicity(1, None)
  val Optional = Multiplicity(0, Some(1))
  def apply(minOccurs: Int, maxOccurs: Int) = new Multiplicity(minOccurs, Some(maxOccurs))
}

sealed trait Input
case class Text(maxLength: Option[Int] = None, pattern: Option[String] = None) extends Input
case class Date(notBefore: Option[LocalDate]) extends Input

sealed trait Element

/**
  * Element describing value of single type SV
  * @tparam SV
  */
sealed trait TemplateElement[+SV <: SValue] extends Element {
  self =>
  val label: String;
  val multiplicity: Multiplicity;

  def emptySingle: SV

  def empty: MValue
}

case class Field(override val label: String, desc: Input, override val multiplicity: Multiplicity = Multiplicity.Once)
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

object SingleFieldValue {
  val Empty = SingleFieldValue(None)

  def apply(s: String): SingleFieldValue = new SingleFieldValue(Some(s))
}

case class Template(name: String, body: Group) {
  def empty = body.emptySingle
}
