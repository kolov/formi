package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._
// Entry in a form
trait DocumentEntry

case class FieldDocumentEntry(label: String, value: Option[String]) extends DocumentEntry
case class GroupDocumentEntry(label: String, entries: Seq[SingleGroupDocumentEntry]) extends DocumentEntry
case class SingleGroupDocumentEntry(label: String, entries: Seq[DocumentEntry]) extends DocumentEntry

object DocumentEntry {

  def render(el: TemplateElement, value: Value): Either[DocumentError, DocumentEntry] = (el, value) match {
    case (f @ Field(_, _), fv @ FieldValue(_)) => renderField(f, fv)
    case (g @ Group(_, _, _), fv @ GroupValue(vals)) =>
      val allEntries: Either[DocumentError, Vector[SingleGroupDocumentEntry]] = vals
        .map(sgv => renderSingleGroup(g, sgv))
        .sequence
      allEntries.map(vals => GroupDocumentEntry(g.label, vals))
    case _ => BadValue().asLeft
  }
  def renderField(field: Field, fieldValue: FieldValue) = FieldDocumentEntry(field.label, fieldValue.value).asRight

  def renderSingleGroup(group: Group, values: SingleGroupValue): Either[DocumentError, SingleGroupDocumentEntry] = {
    val entries: Either[DocumentError, List[DocumentEntry]] = group.fields.map { te =>
      values.values.get(te.label).map(Right(_)).getOrElse(Left(PathError("No such name"))).flatMap { vals =>
        render(te, vals)
      }
    }.sequence
    entries.map(e => SingleGroupDocumentEntry(group.label, e))
  }
}
