package com.akolov.formi.html

import com.akolov.formi.lenses.{GroupInstancePath, GroupOrFieldPath, Indexed, Path}
import com.akolov.formi.{FieldView, GroupView, LabelsProvider, SingleGroupView}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FormiHtmlTest extends AnyFlatSpecLike with Matchers {
  val printer = Printer.Default

  val labelsProvider = new LabelsProvider {
    override def findLabel(els: Seq[String]): Option[String] = Some(s"""{${els.mkString(".")}}""")
  }

  implicit class stringOp(s: String) {
    def stripAll: String = s.replaceAll("[(\\s)*|\\|]", "")
  }

  "renderer" should "render text field" in {
    val fieldElement = FieldView("name", Some("George Costanza"))
    val rendered: Div =
      FormiHtml.renderField(GroupInstancePath(List(Indexed("xx", 0))), fieldElement).run(labelsProvider)

    printer.print(rendered).stripAll shouldEqual """<div class="cu-field cu-field-name">
                                          |  <div class="cu-field-label">{xx.name}</div>
                                          |  <div class="cu-field-value">George Costanza</div>
                                          |</div>""".stripAll
  }

  "renderer" should "render Group" in {
    val sge = SingleGroupView(
      "cv",
      List(
        GroupView(
          "head",
          List(
            SingleGroupView(
              "head",
              List(FieldView("firstName", Some("George")), FieldView("lastName", Some("Costanza"))))))
      )
    )

    val rendered: Div =
      FormiHtml.renderForm(sge, labelsProvider)

    println(printer.print(rendered))
    printer
      .print(rendered)
      .stripAll shouldEqual """<div class="cu-group-instance cu-group-index-0 cu-group-instance-cv">
                                                   |  <div class="cu-group-instance-label">{cv}</div>
                                                   |  <div class="cu-group cu-group-head">
                                                   |    <div class="cu-group-label">{cv.head}</div>
                                                   |    <div class="cu-group-instance cu-group-index-0 cu-group-instance-head">
                                                   |      <div class="cu-group-instance-label">{cv.head}</div>
                                                   |      <div class="cu-field cu-field-firstName">
                                                   |        <div class="cu-field-label">{cv.head.firstName}</div>
                                                   |        <div class="cu-field-value">George</div>
                                                   |      </div>
                                                   |      <div class="cu-field cu-field-lastName">
                                                   |        <div class="cu-field-label">{cv.head.lastName}</div>
                                                   |        <div class="cu-field-value">Costanza</div>
                                                   |      </div>
                                                   |    </div>
                                                   |  </div>
                                                   |</div>
                                                   |""".stripAll
  }
}
