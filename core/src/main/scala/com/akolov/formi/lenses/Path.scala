package com.akolov.formi.lenses

import com.akolov.formi.errors.{DocumentError, PathError}
import cats.implicits._
import scala.util.matching.Regex

sealed trait PathElement
case class Indexed(name: String, index: Int = 0) extends PathElement
case class Named(name: String) extends PathElement
case class Path(groups: Seq[Indexed], named: Option[Named])

object Path {
  def apply(ixs: Indexed*) = new Path(ixs, None)
  def apply(name: String) = new Path(List(), Some(Named(name)))
  def apply(g1: String, ix1: Int, f1: String) = new Path(List(Indexed(g1, ix1)), Some(Named(f1)))

  def parsePath(path: String): Either[DocumentError, Path] = {
    def fromPathElements(els: List[PathElement]): Either[DocumentError, Path] = {
      val initial: Either[DocumentError, (List[Indexed], Option[Named])] = (List.empty[Indexed], None).asRight
      els
        .foldLeft(initial) {
          case (acc, el) =>
            (acc, el) match {
              case (l @ Left(_), _) => l
              case (Right((l, None)), el: Indexed) => (l :+ el, None).asRight
              case (Right((l, None)), named: Named) => (l, Some(named)).asRight
              case (Right((_, Some(_))), _) => PathError(s"Invalid path $els").asLeft
            }
        }
        .flatMap {
          case (l, n @ Some(_)) => new Path(l, n).asRight
          case _ => PathError().asLeft
        }
    }

    val indexedPattern: Regex = """([a-zA-Z0-9_]*)\[(\d+)\]""".r

    val pathSegments: Seq[Either[DocumentError, PathElement]] = path
      .split("/")
      .toList
      .filter(_.length > 0)
      .map { s =>
        s match {
          case indexedPattern(p, i) =>
            i.toIntOption.fold(PathError(s"Not an int: $i").asLeft[Indexed])(in => Indexed(p, in).asRight)
          case n => Named(n).asRight
        }
      }
    pathSegments.toList.sequence.flatMap { els =>
      fromPathElements(els)
    }
  }
}
