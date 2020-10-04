package com.akolov.formi.lenses

import com.akolov.formi.errors.{DocumentError, PathError}
import cats.implicits._
import scala.util.matching.Regex

sealed trait PathElement
case class Indexed(name: String, index: Int = 0) extends PathElement
case class Named(name: String) extends PathElement

case class Path(groups: Seq[Indexed], named: Option[Named]) {
  def depth = groups.size + named.map(_ => 1).getOrElse(0)
}

object Path {
  def apply(ixs: Indexed*) = new Path(ixs, None)
  def apply(name: String) = new Path(List(), Some(Named(name)))
  def apply(g1: String, ix1: Int, f1: String) = new Path(List(Indexed(g1, ix1)), Some(Named(f1)))

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
          case (groups, named) => new Path(groups, named)
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
          case namedPattern => Named(s).asRight
          case x => PathError(s"Invalid path element name: $x").asLeft[Indexed]
        }
      }
    pathSegments.toList.sequence.flatMap { els =>
      fromPathElements(els)
    }
  }
}
