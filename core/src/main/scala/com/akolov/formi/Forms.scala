package com.akolov.formi

import com.akolov.formi.errors._
import cats.implicits._
import com.akolov.formi.lenses.{GroupInstancePath, GroupOrFieldPath, Path}
import org.log4s.getLogger

// Entry in a form
sealed trait Entry
case class FieldEntry(label: String, input: InputDesc, value: FieldValue, desc: Option[String] = None) extends Entry

case class GroupEntry(
  label: String,
  multiplicity: Multiplicity,
  groupInstances: Seq[Seq[Entry]],
  desc: Option[String] = None)
    extends Entry

object EntryForm {
  val logger = getLogger

  private def render(
    prefix: Seq[String],
    templateElement: TemplateElement,
    elementValue: Value,
    labelsProvider: LabelsProvider): Either[DocumentError, Entry] =
    (templateElement, elementValue) match {
      case (field: Field, fieldValue: FieldValue) => renderField(path, field, fieldValue).run(prov).asRight
      case (group: Group, GroupValue(singleGroupValues)) =>
        singleGroupValues
          .map(sgv => renderSingleGroup(group, sgv, labelsProvider))
          .toList
          .sequence
          .map { vals =>
            val label = labelsProvider.getLabel(prefix, group.label)
            GroupEntry(label, group.multiplicity, vals, group.desc)
          }
        }.toList.sequence.map { vals =>
          val translated = prov.getLabel(path.appendGroup(group.label))
          GroupEntry(group.label, translated, group.multiplicity, vals, group.desc)
        }
      case _ => BadValue().asLeft
    }
  }

  private def renderField(field: Field, fieldValue: FieldValue) =
    FieldEntry(field.label, field.input, fieldValue, field.desc).asRight

  def renderSingleGroup(
    group: Group,
    singleGroupValue: SingleGroupValue,
    labelsProvider: LabelsProvider): Either[DocumentError, List[Entry]] = {
    group.fields.map { templateElement =>
      val elementValue: Either[DocumentError, Value] =
        singleGroupValue.values
          .get(templateElement.label)
          .map(Right(_))
          .getOrElse(Left(BadValue(s"No value for label: ${templateElement.label}")))
      elementValue.flatMap { elementValue =>
        val rendered = render(Seq.empty, templateElement, elementValue, labelsProvider)
        logger.debug(s"""Rendered for ${templateElement.label}, ${elementValue}:
             $rendered""")
        rendered
      }
    }.sequence
  }

  def renderEntryForm(
    prov: LabelsProvider,
    group: Group,
    singleGroupValue: SingleGroupValue): Either[DocumentError, List[Entry]] =
    renderSingleGroup(Path(group.label), group, 0, singleGroupValue).run(prov)
}
