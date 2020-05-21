package com.akolov.formi.html

import com.akolov.formi.Rendered.{FieldElement, GroupElement, SingleGroupElement}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FormiHtmlTest extends AnyFlatSpecLike with Matchers {
  val printer = Printer.Default

  implicit class stringOp(s: String) {
    def stripAll = s.replaceAll("[(\\s)*|\\|]", "")
  }

  "renderer" should "render text field" in {
    val fieldElement = FieldElement("name", Some("George Costanza"))
    val rendered: Div = FormiHtml.renderField(fieldElement)

    printer.print(rendered).stripAll shouldEqual """<div class="cu-field cu-field-name-name">
                                          |  <div class="cu-field-label">name</div>
                                          |  <div class="cu-field-value">George Costanza</div>
                                          |</div>""".stripAll
  }

  "renderer" should "render Group" in {
    val sge = SingleGroupElement(
      "cv",
      List(
        GroupElement(
          "head",
          List(
            SingleGroupElement(
              "head",
              List(FieldElement("firstName", Some("George")), FieldElement("lastName", Some("Costanza"))))))
      )
    )

    val rendered: Div = FormiHtml.renderSingleGroup(sge, 0)

    println(printer.print(rendered))
    printer
      .print(rendered)
      .stripAll shouldEqual """<div class="cu-group-instance cu-group-index-0 cu-group-name-cv">
                                                   |  <div class="cu-group-instance-label">cv</div>
                                                   |  <div class="cu-group cu-group-head">
                                                   |    <div class="cu-group-label">head</div>
                                                   |    <div class="cu-group-instance cu-group-index-0 cu-group-name-head">
                                                   |      <div class="cu-group-instance-label">head</div>
                                                   |      <div class="cu-field cu-field-name-firstName">
                                                   |        <div class="cu-field-label">firstName</div>
                                                   |        <div class="cu-field-value">George</div>
                                                   |      </div>
                                                   |      <div class="cu-field cu-field-name-lastName">
                                                   |        <div class="cu-field-label">lastName</div>
                                                   |        <div class="cu-field-value">Costanza</div>
                                                   |      </div>
                                                   |    </div>
                                                   |  </div>
                                                   |</div>
                                                   |""".stripAll
  }
}
