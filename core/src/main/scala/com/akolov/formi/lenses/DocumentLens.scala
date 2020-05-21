package com.akolov.formi.lenses

import cats.implicits._
import com.akolov.formi._
import com.akolov.formi.errors._
import org.log4s.getLogger

trait DocumentLenses {
  val logger = getLogger

  type DocumentLens[P, A] = Lens[P, DocumentError, A]

  sealed trait GroupLensOperation
  case object Editing extends GroupLensOperation
  case object Inserting extends GroupLensOperation
  case object Deleting extends GroupLensOperation

  sealed trait SingleGroupLens

  case class GLens(lens: DocumentLens[SingleGroupValue, SingleGroupValue]) extends SingleGroupLens

  case class FLens(lens: DocumentLens[SingleGroupValue, FieldValue]) extends SingleGroupLens

  case class GFLens(gLens: Option[GLens], fLens: Option[FLens]) { self =>

    /*
    Append a new lens at the end.
     */
    def append(l: SingleGroupLens): Either[DocumentError, GFLens] = l match {
      case gl @ GLens(g) =>
        self.fLens match {
          case None =>
            GFLens(gLens.map(gl => GLens(Lens.compose(gl.lens, g))).orElse(Some(gl)), self.fLens).asRight
          case Some(_) => AttemptTOAppendAfterFLens().asLeft
        }
      case f @ FLens(_) =>
        self.fLens match {
          case None => GFLens(self.gLens, Some(f)).asRight
          case Some(_) => AttemptTOAppendAfterFLens().asLeft
        }
    }

    def asFieldLens: Either[DocumentError, DocumentLens[SingleGroupValue, FieldValue]] = (gLens, fLens) match {
      case (Some(GLens(g)), Some(FLens(f))) => Lens.compose(g, f).asRight
      case (Some(_), None) => NotFieldPath("").asLeft
      case (None, Some(f)) => f.lens.asRight
      case (None, None) => NotFieldPath("").asLeft
    }

    def asGroupLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      (gLens, fLens) match {
        case (Some(_), Some(_)) => NotGroupPath().asLeft
        case (Some(g), None) => g.lens.asRight
        case (None, Some(f)) => NotGroupPath().asLeft
        case (None, None) => NotGroupPath().asLeft
      }
  }

  object GFLens {
    def empty = new GFLens(None, None)
  }

  def failingLens[P, A](e: DocumentError): DocumentLens[P, A] = new DocumentLens[P, A] {
    override def get(p: P): Either[DocumentError, A] = Left(e)

    override def set(p: P, a: Option[A]): Either[DocumentError, P] = Left(e)
  }

  def nameLens(groupElement: Group, name: String): Either[DocumentError, (Element, FLens)] =
    groupElement.fields.find(_.label === name) match {
      case Some(ge @ Group(_, _, _)) =>
        IndexError(s"""Field $name. requires index""").asLeft

      case Some(fe @ Field(_, _)) =>
        val lens = new DocumentLens[SingleGroupValue, FieldValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, FieldValue] = {
            p.get(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: FieldValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, a: Option[FieldValue]): Either[DocumentError, SingleGroupValue] =
            a match {
              case Some(sa) => p.update(name, sa)
              case None => BadValue("Attempt to assign None to FieldValue").asLeft
            }
        }
        (fe, FLens(lens)).asRight
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def nameIndexLens(
    groupElement: Group,
    name: String,
    ix: Int,
    op: GroupLensOperation): Either[DocumentError, (Element, GLens)] =
    groupElement.fields.find(_.label === name) match {
      case Some(ge @ Group(_, _, _)) =>
        val lens = new DocumentLens[SingleGroupValue, GroupValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, GroupValue] = {
            p.get(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: GroupValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, a: Option[GroupValue]): Either[DocumentError, SingleGroupValue] =
            a match {
              case Some(sa) => p.update(name, sa)
              case None => BadValue("Attempt to assign None to FieldValue").asLeft
            }
        }
        val composed = Lens.compose(lens, indexLens(ge, ix, op))
        (ge, GLens(composed)).asRight
      case Some(_ @Field(_, _)) =>
        IndexError(s"""Field $name can't have index""").asLeft
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def indexLens(element: Group, ix: Int, op: GroupLensOperation): DocumentLens[GroupValue, SingleGroupValue] =
    new DocumentLens[GroupValue, SingleGroupValue] {

      override def get(p: GroupValue): Either[DocumentError, SingleGroupValue] = {
        if (ix < p.singleGroups.length) p.singleGroups(ix).asRight
        else IndexError(s"Can't get at index $ix with multiplicity ${element.multiplicity}").asLeft
      }

      override def set(p: GroupValue, a: Option[SingleGroupValue]): Either[DocumentError, GroupValue] = {
        op match {
          case Editing =>
            if (ix < p.singleGroups.length) {
              val before = p.singleGroups.take(ix).toVector
              val after = p.singleGroups.drop(ix + 1).toVector
              a match {
                case Some(sa) => GroupValue((before :+ sa) ++ after).asRight
                case None => BadValue("Attemt to Edit None").asLeft
              }
            } else
              IndexError(s"Can't set at index $ix: ${p.singleGroups.length} elements").asLeft

          case Inserting =>
            if (ix <= p.singleGroups.length) {
              val before = p.singleGroups.take(ix).toVector
              val after = p.singleGroups.drop(ix + 1).toVector
              GroupValue((before :+ a.getOrElse(element.singleEmpty)) ++ after).asRight
            } else
              IndexError(s"Can't insert at index $ix: ${p.singleGroups.length} elements").asLeft

          case Deleting =>
            if (ix <= p.singleGroups.length - 1) {
              val before = p.singleGroups.take(ix).toVector
              val after = p.singleGroups.drop(ix + 1).toVector
              GroupValue(before ++ after).asRight
            } else
              IndexError(s"Can't insert at index $ix: ${p.singleGroups.length} elements").asLeft
        }
      }
    }

  def lensFor(groupElement: Group, path: Path, op: GroupLensOperation = Editing): Either[DocumentError, GFLens] = {
    case class Acc(templateElement: TemplateElement, lens: Either[DocumentError, GFLens])

    val acc: (TemplateElement, Either[DocumentError, Option[GLens]]) =
      path.groups.foldLeft((groupElement: TemplateElement, None.asRight: Either[DocumentError, Option[GLens]])) {
        // if error, shortcut
        case (acc @ (_, Left(_)), _) => acc
        // Firs element, group
        case ((ge: Group, Right(otLens)), indexed) =>
          logger.debug(s"Enter lens calculation iteration at element: ${ge.label}, path: $indexed")
          nameIndexLens(ge, indexed.name, indexed.index, op) match {
            case Right((te: TemplateElement, glens: GLens)) =>
              logger.debug(s"outcome: Group, nextTemplate: ${te.label} lens: ${glens}")
              otLens match {
                case None => (te, Some(glens).asRight)
                case Some(l) => (te, Some(GLens(Lens.compose(l.lens, glens.lens))).asRight)
              }
            case Left(e) =>
              logger.debug(s"LeftOutcome")
              (ge, Left(e))
            case nameIndexLens => (ge, InternalError(s"Unexpected: $nameIndexLens").asLeft)
          }
      }
    acc match {
      case (_, Left(e)) => Left(e)
      case (ge @ Group(l, fs, m), Right(glens)) =>
        path.named match {
          case Some(Named(name)) =>
            nameLens(ge, name).map {
              case (_, flens) =>
                GFLens(glens, Some(flens))
            }
          case None => GFLens(glens, None).asRight
        }
      case (_: Field, _) => InternalError("Field: unexpected").asLeft
    }
  }

  def fieldLensFor(groupElement: Group, path: Path): Either[DocumentError, DocumentLens[SingleGroupValue, FieldValue]] =
    lensFor(groupElement, path, Editing).flatMap(_.asFieldLens)

  def groupLensFor(
    groupElement: Group,
    path: Path,
    op: GroupLensOperation = Editing): Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
    lensFor(groupElement, path, op).flatMap(_.asGroupLens)
}
object DocumentLenses extends DocumentLenses
