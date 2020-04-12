package com.akolov.formi

import com.akolov.formi.errors._
import org.log4s.getLogger
import cats.implicits._

sealed trait SValue
sealed trait MValue
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

object SingleFieldValue {
  val Empty = SingleFieldValue(None)

  def apply(s: String): SingleFieldValue = new SingleFieldValue(Some(s))
}

case class Template(name: String, body: Group) {
  def empty = body.emptySingle
}
