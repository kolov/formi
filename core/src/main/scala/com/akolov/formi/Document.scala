package com.akolov.formi

// Entry in a form
trait EntryElement

case class SingleField(label: String, desc: InputDesc)

trait Renderer[A] {
  def render(a: A): EntryElement
}

object EntryInstances {

  implicit val SingleFieldRendered = new Renderer[SingleField] {
    override def render(a: SingleField): EntryElement = ???
  }
}
