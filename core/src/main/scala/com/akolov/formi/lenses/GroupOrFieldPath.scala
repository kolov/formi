package com.akolov.formi.lenses

import com.akolov.formi.errors.{DocumentError, PathError}
import cats.implicits._
import scala.util.matching.Regex

sealed trait PathElement
case class Indexed(name: String, index: Int = 0) extends PathElement
case class Named(name: String) extends PathElement

sealed trait Path {
  def asStrings: Seq[String]
}

object Path {
  val empty = new GroupInstancePath(Seq.empty)
  def apply(name: String): GroupOrFieldPath = new GroupOrFieldPath(Seq.empty, Named(name))
  def apply(groups: Seq[Indexed], name: String): GroupOrFieldPath = new GroupOrFieldPath(groups, Named(name))

  def apply(groups: Seq[Indexed], name: Option[Named]): Path = name match {
    case Some(n) => new GroupOrFieldPath(groups, n)
    case None => new GroupInstancePath(groups)
  }

  def parsePath(path: String): Either[DocumentError, Path] = {
    def fromPathElements(patheElements: List[PathElement]): Either[DocumentError, Path] = {
      val initial: Either[DocumentError, (List[Indexed], Option[Named])] = (List.empty[Indexed], None).asRight
      patheElements
        .foldLeft(initial) {
          case (acc, el) =>
            (acc, el) match {
              case (l @ Left(_), _) => l
              case (Right((l, None)), el: Indexed) => (l :+ el, None).asRight
              case (Right((l, None)), named: Named) => (l, Some(named)).asRight
              case (Right((_, Some(_))), _) => PathError(s"Invalid path $patheElements").asLeft
            }
        }
        .map {
          case (groups, named) => Path(groups, named)
        }
    }

    val indexedPattern: Regex = """([a-zA-Z0-9_-]*)\[(\d+)\]""".r
    val namedPattern: Regex = """[a-zA-Z0-9_-]*""".r

    val pathSegments: Seq[Either[DocumentError, PathElement]] = path
      .split("/")
      .toList
      .filter(_.length > 0)
      .map { s =>
        s match {
          case indexedPattern(p, i) =>
            i.toIntOption.fold(PathError(s"Not an int: $i").asLeft[Indexed])(in => Indexed(p, in).asRight)
          case x if namedPattern.matches(x) => Named(x).asRight
          case x => PathError(s"Invalid path element name: $x").asLeft[Indexed]
        }
      }
    pathSegments.toList.sequence.flatMap { els =>
      fromPathElements(els)
    }
  }
}

case class GroupInstancePath(groups: Seq[Indexed]) extends Path {
  def appendField(label: String): Path = new GroupOrFieldPath(groups, Named(label))

  def appendGroup(label: String): GroupOrFieldPath = new GroupOrFieldPath(groups, Named(label))
  def appendGroupAt(label: String, ix: Int): GroupInstancePath = new GroupInstancePath(groups :+ Indexed(label, ix))

  override def asStrings: Seq[String] = groups.map(_.name)
}

object GroupInstancePath {
  val empty = new GroupInstancePath(Seq.empty)
}

case class GroupOrFieldPath(groups: Seq[Indexed], named: Named) extends Path {
  def depth: Int = groups.size + 1

  def at(ix: Int) = new GroupInstancePath(groups :+ Indexed(named.name, ix))

  override def asStrings: Seq[String] = groups.map(_.name) :+ named.name
}

object GroupOrFieldPath {
  def apply(name: String) = new GroupOrFieldPath(List(), Named(name))
  def apply(g1: String, ix1: Int, f1: String) = new GroupOrFieldPath(List(Indexed(g1, ix1)), Named(f1))
}
