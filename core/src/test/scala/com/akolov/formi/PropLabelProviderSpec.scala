package com.akolov.formi

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class PropLabelProviderSpec extends AnyFlatSpecLike with Matchers {
  val props = Map("a.b.c" -> "abc", "c" -> "c")
  "labels selector" should "select" in {
    val selector = PropLabelProvider.make.run(props)

    selector.getLabel(Seq("a","b","c"), "d") shouldBe "abc"
    selector.getLabel(Seq("a","b"), "c") shouldBe "abc"
    selector.getLabel(Seq("a"), "c") shouldBe "c"
    selector.getLabel(Seq("c"), "fff") shouldBe "c"
  }
}
