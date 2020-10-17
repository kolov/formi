package com.akolov.formi

import java.io.ByteArrayInputStream

import cats.data.Reader
import com.akolov.formi.lenses.{GroupInstancePath, Path}

import scala.jdk.CollectionConverters.SetHasAsScala
import scala.util.Try

trait LabelsProvider {
  def findLabel(els: Seq[String]): Option[String]

  def getLabel(prefix: Seq[String], name: String): String = findLabel(prefix :+ name).getOrElse(name)

  def getLabel(path: Path): String = {
    val els: Seq[String] = path.asStrings
    findLabel(els).orElse(els.lastOption).getOrElse("")
  }
}

object LabelsProvider {

  val echo = new LabelsProvider {
    override def findLabel(els: Seq[String]): Option[String] = Some(els.mkString("."))
  }

  def translate(path: GroupInstancePath, label: String): Reader[LabelsProvider, String] =
    Reader[LabelsProvider, String] { prov =>
      prov.getLabel(path.appendGroup(label))
    }

  def translate(path: Path): Reader[LabelsProvider, String] =
    Reader[LabelsProvider, String] { prov =>
      prov.getLabel(path)
    }
}

object PropLabelProvider {

  def fromMap(props: Map[String, String]): LabelsProvider =
    new LabelsProvider {

      override def findLabel(els: Seq[String]): Option[String] = {
        els.tails.to(LazyList).map(t => props.get(t.mkString("."))).collect { case Some(s) => s }.headOption
      }
    }

  def fromPropertiesContent(propertiesContent: String): Either[Throwable, LabelsProvider] = {
    Try {
      val props = new java.util.Properties()
      props.load(new ByteArrayInputStream(propertiesContent.getBytes))
      props
    }.map { props =>
      fromMap(props.entrySet.asScala.map { e =>
        (e.getKey.toString, e.getValue.toString)
      }.toMap)
    }.toEither
  }
}
