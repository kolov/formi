package com.akolov.formi

import cats.data.Reader
import com.akolov.formi.lenses.{GroupInstancePath, Path}

trait LabelsProvider {
  def findLabel(els: Seq[String]): Option[String]

  def getLabel(prefix: Seq[String], name: String): String = findLabel(prefix :+ name).getOrElse(name)

  def getLabel(path: Path): String = {
    val els: Seq[String] = path.asString
    findLabel(els).orElse(els.lastOption).getOrElse("")
  }
}


object LabelsProvider {
  def translate(path: GroupInstancePath, label: String)  : Reader[LabelsProvider, String] = Reader[LabelsProvider, String] { prov =>
    prov.getLabel(path.appendGroup(label))
  }
}

object PropLabelProvider {

  def make: Reader[Map[String, String], LabelsProvider] = {
    Reader { props =>
      new LabelsProvider {
        override def findLabel(els: Seq[String]) = {
          els.inits.to(LazyList).map(t => props.get(t.mkString("."))).collect { case Some(s) => s }.headOption
        }
      }
    }
  }
}
