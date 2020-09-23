package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._
import org.log4s.getLogger

// Entry in a form
sealed trait Entry
case class FieldEntry(label: String, desc: InputDesc, value: FieldValue) extends Entry
case class GroupEntry(label: String, multiplicity: Multiplicity, groupInstances: Seq[Seq[Entry]]) extends Entry

object EntryForm {
  val logger = getLogger

  private def render(templateElement: TemplateElement, elementValue: Value): Either[DocumentError, Entry] =
    (templateElement, elementValue) match {
      case (field: Field, fieldValue: FieldValue) => renderField(field, fieldValue)
      case (group: Group, GroupValue(singleGroupValues)) =>
        singleGroupValues
          .map(sgv => renderSingleGroup(group, sgv))
          .toList
          .sequence
          .map { vals =>
            GroupEntry(group.label, group.multiplicity, vals)
          }
      case _ => BadValue().asLeft
    }

  private def renderField(field: Field, fieldValue: FieldValue) =
    FieldEntry(field.label, field.desc, fieldValue).asRight

  def renderSingleGroup(group: Group, singleGroupValue: SingleGroupValue) = {
    group.fields.map { templateElement =>
      val elementValue: Either[DocumentError, Value] =
        singleGroupValue.values
          .get(templateElement.label)
          .map(Right(_))
          .getOrElse(Left(BadValue(s"No value for label: ${templateElement.label}")))
      elementValue.flatMap { elementValue =>
        val rendered = render(templateElement, elementValue)
        logger.debug(s"""Rendered for ${templateElement.label}, ${elementValue}:
             $rendered""")
        rendered
      }
    }.sequence
  }
}
