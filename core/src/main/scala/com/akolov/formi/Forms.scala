package com.akolov.formi

import cats.data.Reader
import com.akolov.formi.errors._
import cats.implicits._
import com.akolov.formi.lenses.{GroupInstancePath, GroupOrFieldPath, Path}
import org.log4s.getLogger

// Entry in a form
sealed trait Entry

case class FieldEntry(
  label: String,
  translatedLabel: String,
  input: InputDesc,
  value: FieldValue,
  desc: Option[String] = None)
    extends Entry

case class GroupEntry(
  label: String,
  translatedLabel: String,
  multiplicity: Multiplicity,
  groupInstances: Seq[Seq[Entry]],
  desc: Option[String] = None)
    extends Entry

object EntryForm {
  val logger = getLogger

  private def render(
    path: GroupInstancePath,
    templateElement: TemplateElement,
    elementValue: Value): Reader[LabelsProvider, Either[DocumentError, Entry]] = Reader { prov =>
    (templateElement, elementValue) match {
      case (field: Field, fieldValue: FieldValue) => renderField(path, field, fieldValue).run(prov).asRight
      case (group: Group, GroupValue(singleGroupValues)) =>
        singleGroupValues
          .map(sgv => renderSingleGroup(group, sgv))
          .toList
          .sequence
          .map { vals =>
            GroupEntry(group.label, group.multiplicity, vals)
          }
        }.toList.sequence.map { vals =>
          val translated = prov.getLabel(path.appendGroup(group.label))
          GroupEntry(group.label, translated, group.multiplicity, vals, group.desc)
        }
      case _ => BadValue().asLeft
    }
  }

  private def renderField(
    path: GroupInstancePath,
    field: Field,
    fieldValue: FieldValue): Reader[LabelsProvider, FieldEntry] = Reader { prov =>
    FieldEntry(field.label, prov.getLabel(path.appendField(field.label)), field.input, fieldValue, field.desc)
  }

  private def renderSingleGroup(
    path: GroupOrFieldPath,
    group: Group,
    ix: Int,
    singleGroupValue: SingleGroupValue): Reader[LabelsProvider, Either[DocumentError, List[Entry]]] = Reader { prov =>
    group.fields.map { templateElement =>
      val elementValue: Either[DocumentError, Value] =
        singleGroupValue.values
          .get(templateElement.label)
          .map(Right(_))
          .getOrElse(Left(BadValue(s"No value for label: ${templateElement.label}")))
      elementValue.flatMap { elementValue =>
        render(path.at(ix), templateElement, elementValue).run(prov)
      }
    }.sequence
  }

  def renderEntryForm(
    prov: LabelsProvider,
    group: Group,
    singleGroupValue: SingleGroupValue): Either[DocumentError, List[Entry]] =
    renderSingleGroup(Path(group.label), group, 0, singleGroupValue).run(prov)
}
