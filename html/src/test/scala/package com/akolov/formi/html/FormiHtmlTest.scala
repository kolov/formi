package com.akolov.formi.html

import com.akolov.formi.Rendered.FieldElement
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FormiHtmlTest extends AnyFlatSpecLike with Matchers {
  val printer = Printer.Default

  "renderer" should "render text field" in {
    val fieldElement = FieldElement("name", Some("George Costanza"))
    val rendered: Div = FormiHtml.renderField(fieldElement)

    printer.print(rendered) shouldEqual """<div class="field-name-name">
                                          |  <div class="field-label">name</div>
                                          |  <div class="field-value">George Costanza</div>
                                          |</div>""".stripMargin
  }
}
