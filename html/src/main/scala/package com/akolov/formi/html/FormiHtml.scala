package com.akolov.formi.html

import com.akolov.formi.html.Div.{Children, Content, TextContent}
import com.akolov.formi.{FieldView, GroupView, SingleGroupView, Rendered => R}

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

  def print(text: TextContent, dx: Int): String = text.text

  def print(content: Content, dx: Int): String = content match {
    case c: Children => print(c, dx)
    case t: TextContent => print(t, dx)
  }
}

object FormiHtml {

  implicit class FieldOps(f: FieldView) {
    def getContent: String = f.value.getOrElse("")
  }

  def renderSingleGroup(g: SingleGroupView, ix: Int): Div = {
    val groupInstanceLabelDiv = Div(List("group-instance-label"), TextContent(g.label))
    val children = g.entries.map {
      case g @ GroupView(_, _) => renderGroup(g)
      case f @ FieldView(_, _) => renderField(f)
    }
    Div(
      List(s"group-instance", s"group-index-$ix", s"group-name-${g.label}"),
      Children(groupInstanceLabelDiv +: children))
  }

  def renderGroup(g: GroupView): Div = {
    val groupLabelDiv = Div(List("group-label"), TextContent(g.label))
    val grupInstances = g.entries.zipWithIndex.map {
      case (e, ix) => renderSingleGroup(e, ix)
    }

    Div(
      List(s"group", s"group-${g.label}"),
      Children(groupLabelDiv +: grupInstances)
    )
  }

  def renderField(f: FieldView) =
    Div(
      List(s"field", s"field-name-${f.label}"),
      Children(
        List(Div(List("field-label"), TextContent(f.label)), Div(List("field-value"), TextContent(f.getContent))))
    )
}
