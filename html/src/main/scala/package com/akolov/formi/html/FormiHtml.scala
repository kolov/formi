package com.akolov.formi.html

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import com.akolov.formi.{Document, Field, FieldValue, Group, GroupValue, SingleGroupValue, TemplateElement, Value}
import cats.implicits._
import com.akolov.formi.html.Div.{Children, Content, TextContent}
import com.akolov.formi.{Rendered => R}

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

class PlainPrinter(indent: Int) extends Printer {
  val newLine = if (indent == 0) "" else "\n"
  val space: String = List.fill(indent)(' ').mkString("")

  override def print(div: Div): String =
    s"""<div class="${div.classes.mkString(" ")}">${print(div.content)}</div>""".stripMargin

  def print(children: Children): String =
    children.children.map { c =>
      s"""$newLine$space${print(c)}""".stripMargin
    }.mkString("") + newLine

  def print(text: TextContent): String = text.text

  def print(content: Content): String = content match {
    case c: Children => print(c)
    case t: TextContent => print(t)
  }
}

object FormiHtml {

  implicit class FieldOps(f: R.FieldElement) {
    def getContent: String = f.value.getOrElse("")
  }

  implicit val renderableDiv = new Renderable[Div] {
    override def render(a: Div): String = "div"
  }

  def renderSingleGroup(g: R.SingleGroupElement) =
    Div(
      List(s"sgroup", s"group-name-${g.label}"),
      Children(List(Div(List("field-label"), TextContent(g.label)), Div(List("field-value"), TextContent("")))))

  def renderField(f: R.FieldElement) =
    Div(
      List(s"field-name-${f.label}"),
      Children(
        List(Div(List("field-label"), TextContent(f.label)), Div(List("field-value"), TextContent(f.getContent)))))
}
