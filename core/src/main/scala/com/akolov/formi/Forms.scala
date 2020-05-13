package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._
// Entry in a form
sealed trait Entry
case class FieldEntry(label: String, desc: InputDesc, value: FieldValue) extends Entry
case class GroupEntry(label: String, multiplicity: Multiplicity, groupInstances: Seq[Seq[Entry]]) extends Entry

object EntryForm {

  private def render(el: TemplateElement, value: Value): Either[DocumentError, Entry] = (el, value) match {
    case (f: Field, fv: FieldValue) => renderField(f, fv)
    case (g: Group, GroupValue(singleGroups)) =>
      singleGroups
        .map(sgv => renderSingleGroup(g, sgv))
        .toList
        .sequence
        .map { vals =>
          GroupEntry(g.label, g.multiplicity, vals)
        }
    case _ => BadValue().asLeft
  }

  private def renderField(field: Field, fieldValue: FieldValue) =
    FieldEntry(field.label, field.desc, fieldValue).asRight

  def renderSingleGroup(group: Group, values: SingleGroupValue) = {
    group.fields.map { te =>
      val v: Either[PathError, Value] =
        values.values.get(te.label).map(Right(_)).getOrElse(Left(PathError("No such name")))
      v.flatMap { vv =>
        render(te, vv)
      }
    }.sequence
  }
}
