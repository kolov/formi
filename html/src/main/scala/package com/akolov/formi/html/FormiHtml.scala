package com.akolov.formi.html

import cats.data.Reader
import com.akolov.formi.html.Div.{Children, Content, TextContent}
import com.akolov.formi.lenses.{GroupInstancePath, GroupOrFieldPath, Path}
import com.akolov.formi.{FieldView, GroupView, LabelsProvider, SingleGroupView, Rendered => R}
import cats.implicits._

object Div {
  sealed trait Content
  case class Children(children: Seq[Div]) extends Content
  case class TextContent(text: String) extends Content
}
case class Div(classes: Seq[String], content: Content)

trait Renderable[A] {
  def render(a: A): String
}

trait Printer {
  def print(div: Div): String
}

object Printer {
  val Default = new PlainPrinter(2)
}

class PlainPrinter(indent: Int, prefix: String = "cu-") extends Printer {
  val newLine = if (indent == 0) "" else "\n"

  def spaces(n: Int): String = List.fill(n)(' ').mkString("")

  override def print(div: Div): String = print(div, 0)

  def print(div: Div, dx: Int): String = {
    val classes = div.classes.map(c => s"$prefix$c").mkString(" ")
    s"""${spaces(dx)}<div class="${classes}">${print(div.content, dx + indent)}</div>""".stripMargin
  }

  def print(children: Children, dx: Int): String =
    children.children.map { c =>
      s"""$newLine${print(c, dx)}""".stripMargin
    }.mkString("") + s"$newLine${spaces(dx - indent)}"

  def print(text: TextContent, dx: Int): String = text.text.replaceAll("\n", "<br>")

  def print(content: Content, dx: Int): String = content match {
    case c: Children => print(c, dx)
    case t: TextContent => print(t, dx)
  }
}

object FormiHtml {

  implicit class FieldOps(f: FieldView) {
    def getContent: String = f.value.getOrElse("")
  }

  type Labels = Map[String, String]

  def renderForm(sgv: SingleGroupView, labelsProvider: LabelsProvider): Div =
    renderSingleGroup(GroupOrFieldPath(sgv.label), sgv, 0).run(labelsProvider)

  private[html] def renderSingleGroup(
    path: GroupOrFieldPath,
    sgv: SingleGroupView,
    ix: Int): Reader[LabelsProvider, Div] = {
    for {
      translatedLabel <- LabelsProvider.translate(path)
      children <- sgv.entries.map {
        case g @ GroupView(_, _) => renderGroup(path.at(ix), g)
        case f @ FieldView(_, _) => renderField(path.at(ix), f)
      }.toList.sequence
    } yield Div(
      List(s"group-instance", s"group-index-$ix", s"group-instance-${sgv.label}"),
      Children(Div(List("group-instance-label"), TextContent(translatedLabel)) +: children)
    )
  }

  private[html] def renderGroup(path: GroupInstancePath, g: GroupView): Reader[LabelsProvider, Div] =
    for {
      tranlatedGroupLabel <- LabelsProvider.translate(path, g.label)
      groupLabelDiv = Div(List("group-label"), TextContent(tranlatedGroupLabel))
      groupInstances <- g.entries.zipWithIndex.map {
        case (e, ix) => renderSingleGroup(path.appendGroup(g.label), e, ix)
      }.toList.sequence
    } yield Div(
      List(s"group", s"group-${g.label}"),
      Children(groupLabelDiv +: groupInstances)
    )

  private[html] def renderField(path: GroupInstancePath, f: FieldView): Reader[LabelsProvider, Div] =
    Reader { prov =>
      val label = prov.getLabel(path.appendField(f.label))
      Div(
        List(s"field", s"field-${f.label}"),
        Children(
          List(Div(List("field-label"), TextContent(label)), Div(List("field-value"), TextContent(f.getContent))))
      )
    }
}
