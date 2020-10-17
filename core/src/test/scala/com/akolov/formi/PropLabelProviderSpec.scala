package com.akolov.formi

import com.akolov.formi.lenses.Path
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class PropLabelProviderSpec extends AnyFlatSpecLike with Matchers {
  "Map labels selector" should "select" in {
    val props = Map("a.b.c" -> "abc", "c" -> "c")
    val selector = PropLabelProvider.fromMap(props)

    selector.getLabel(Seq("a", "b", "c"), "d") shouldBe "d"
    selector.getLabel(Seq("a", "b"), "c") shouldBe "abc"
    selector.getLabel(Seq("a"), "c") shouldBe "c"
    selector.getLabel(Seq("c"), "fff") shouldBe "fff"
  }

  "Props labels selector" should "select" in {
    val selector = PropLabelProvider.fromPropertiesContent(content)

//    selector.map(_.getLabel(Path("name"))) shouldBe Right("Name")

    (for {
      path <- Path.parsePath("cv[0]/name")
      provider <- PropLabelProvider.fromPropertiesContent(content)
    } yield provider.getLabel(path)) shouldBe Right("Name")
  }

  val content =
    """
      |name=Name
      |email=E-mail
      |phone=Phone number
      |address=Address
      |intro=Introduction
      |core-skills=Core Skills
      |education=Education
      |certificated=Certificates
      |languages=Languages
      |work-experience=Work Experience
      |start-date=From
      |emd-date=To
      |role=Role
      |technology=Technology
      |work-experience.details=Details about the position""".stripMargin
}
