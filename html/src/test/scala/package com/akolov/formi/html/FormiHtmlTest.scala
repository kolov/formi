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

    printer.print(rendered).stripAll shouldEqual """<div class="field field-name-name">
                                          |  <div class="field-label">name</div>
                                          |  <div class="field-value">George Costanza</div>
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
              List(FieldElement("firstName", Some("George")), FieldElement("lastName", Some("Costanza")))))),
        GroupElement(
          "head",
          List(
            SingleGroupElement(
              "head",
              List(FieldElement("firstName", Some("George")), FieldElement("lastName", Some("Costanza")))))
        )
      )
    )

    val rendered: Div = FormiHtml.renderSingleGroup(sge, 0)

    println(printer.print(rendered))
    printer.print(rendered).stripAll shouldEqual """<div class="group-element group-index-0 group-name-cv">
                                          |  <div class="group group-head">
                                          |    <div class="group-element group-index-0 group-name-head">
                                          |      <div class="field field-name-firstName">
                                          |        <div class="field-label">firstName</div>
                                          |        <div class="field-value">George</div>
                                          |      </div>
                                          |      <div class="field field-name-lastName">
                                          |        <div class="field-label">lastName</div>
                                          |        <div class="field-value">Costanza</div>
                                          |      </div>
                                          |    </div>
                                          |  </div>
                                          |  <div class="group group-head">
                                          |    <div class="group-element group-index-0 group-name-head">
                                          |      <div class="field field-name-firstName">
                                          |        <div class="field-label">firstName</div>
                                          |        <div class="field-value">George</div>
                                          |      </div>
                                          |      <div class="field field-name-lastName">
                                          |        <div class="field-label">lastName</div>
                                          |        <div class="field-value">Costanza</div>
                                          |      </div>
                                          |    </div>
                                          |  </div>
                                          |</div>""".stripAll
  }
}
