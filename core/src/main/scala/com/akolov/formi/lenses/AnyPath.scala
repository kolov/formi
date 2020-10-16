package com.akolov.formi.lenses

import com.akolov.formi.errors.{DocumentError, PathError}
import cats.implicits._
import scala.util.matching.Regex

sealed trait PathElement
case class Indexed(name: String, index: Int = 0) extends PathElement
case class Named(name: String) extends PathElement

sealed trait Path {
  def asString: Seq[String]
}

case class GroupInstancePath(groups: Seq[Indexed]) extends Path {
  def appendField(label: String): Path = new AnyPath(groups, Some(Named(label)))

  def appendGroup(label: String): Path = new AnyPath(groups, Some(Named(label)))

  override def asString: Seq[String] = groups.map(_.name)
}

object GroupInstancePath {
  val empty = new GroupInstancePath(Seq.empty)
}

case class AnyPath(groups: Seq[Indexed], named: Option[Named]) extends Path {
  def depth: Int = groups.size + named.map(_ => 1).getOrElse(0)

  def appendGroup(name: String) = this match {
    case AnyPath(_, Some(_)) => Left(PathError(s"Can't append group to $this"))
    case AnyPath(els, None) => Right(new AnyPath(els, Some(Named(name))))
  }

  override def asString: Seq[String] = groups.map(_.name) ++ named.map(_.name).toList
}

object AnyPath {
  def apply(ixs: Indexed*) = new AnyPath(ixs, None)
  def apply(name: String) = new AnyPath(List(), Some(Named(name)))
  def apply(g1: String, ix1: Int, f1: String) = new AnyPath(List(Indexed(g1, ix1)), Some(Named(f1)))

  val empty = new AnyPath(Seq.empty, None)

  def parsePath(path: String): Either[DocumentError, AnyPath] = {
    def fromPathElements(patheElements: List[PathElement]): Either[DocumentError, AnyPath] = {
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
          case (groups, named) => new AnyPath(groups, named)
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
