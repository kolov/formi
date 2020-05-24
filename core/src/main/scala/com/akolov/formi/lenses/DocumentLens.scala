package com.akolov.formi.lenses

import cats.implicits._
import com.akolov.formi._
import com.akolov.formi.errors._
import org.log4s.getLogger

trait DocumentLenses {
  val logger = getLogger

  type DocumentLens[P, A] = Lens[P, DocumentError, A]

  sealed trait SingleGroupLens

  sealed trait FinalLens extends SingleGroupLens

  case class SGLens(lens: DocumentLens[SingleGroupValue, SingleGroupValue]) extends SingleGroupLens

  case class GLens(lens: DocumentLens[SingleGroupValue, GroupValue]) extends SingleGroupLens with FinalLens

  case class FLens(lens: DocumentLens[SingleGroupValue, FieldValue]) extends SingleGroupLens with FinalLens

  case class GFLens(sgLens: Option[SGLens], fLens: Option[FLens], gLens: Option[GLens]) {
    self =>

    /*
    Append a new lens at the end. Only 1 FLens can be appended`
     */
    def append(l: SingleGroupLens): Either[DocumentError, GFLens] = l match {
      case gl @ SGLens(g) =>
        (self.fLens, self.gLens) match {
          case (None, None) =>
            GFLens(sgLens.map(gl => SGLens(Lens.compose(gl.lens, g))).orElse(Some(gl)), None, None).asRight
          case _ => AttemptTOAppendAfterFLens().asLeft
        }
      case f @ FLens(_) =>
        (self.fLens, self.gLens) match {
          case (None, None) => GFLens(self.sgLens, Some(f), None).asRight
          case _ => AttemptTOAppendAfterFLens().asLeft
        }
      case g @ GLens(_) =>
        (self.fLens, self.gLens) match {
          case (None, None) => GFLens(self.sgLens, None, Some(g)).asRight
          case _ => AttemptTOAppendAfterFLens().asLeft
        }
    }

    def asFieldLens: Either[DocumentError, DocumentLens[SingleGroupValue, FieldValue]] = (sgLens, fLens) match {
      case (Some(SGLens(g)), Some(FLens(f))) => Lens.compose(g, f).asRight
      case (Some(_), None) => NotFieldPath("").asLeft
      case (None, Some(f)) => f.lens.asRight
      case (None, None) => NotFieldPath("").asLeft
    }

    def asSingleGroupLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      (sgLens, fLens) match {
        case (Some(_), Some(_)) => NotGroupPath().asLeft
        case (Some(g), None) => g.lens.asRight
        case (None, Some(_)) => NotGroupPath().asLeft
        case (None, None) => NotGroupPath().asLeft
      }

    def asGroupLens: Either[DocumentError, DocumentLens[SingleGroupValue, GroupValue]] =
      (sgLens, gLens) match {
        case (Some(SGLens(g)), Some(GLens(f))) => Lens.compose(g, f).asRight
        case (Some(_), None) => NotFieldPath("").asLeft
        case (None, Some(f)) => f.lens.asRight
        case (None, None) => NotFieldPath("").asLeft
      }
  }

  object GFLens {
    def empty = new GFLens(None, None, None)
  }

  def failingLens[P, A](e: DocumentError): DocumentLens[P, A] = new DocumentLens[P, A] {
    override def get(p: P): Either[DocumentError, A] = Left(e)

    override def set(p: P, a: A): Either[DocumentError, P] = Left(e)
  }

  def nameLens(group: Group, name: String): Either[DocumentError, (TemplateElement, FinalLens)] =
    group.fields.find(_.label === name) match {
      case Some(ge @ Group(_, _, _)) =>
        val lens = new DocumentLens[SingleGroupValue, GroupValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, GroupValue] = {
            p.getElement(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: GroupValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }
          override def set(p: SingleGroupValue, groupValue: GroupValue): Either[DocumentError, SingleGroupValue] = {
            if (ge.multiplicity.allows(groupValue.singleGroups.size))
              SingleGroupValue(p.values + (name -> groupValue)).asRight
            else
              MultiplicityError().asLeft
          }
        }
        (group, GLens(lens)).asRight

      case Some(fe @ Field(_, _)) =>
        val lens = new DocumentLens[SingleGroupValue, FieldValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, FieldValue] = {
            p.getElement(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: FieldValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, fieldValue: FieldValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, fieldValue)
        }
        (fe, FLens(lens)).asRight
      case None =>
        IndexError(s"""No such name: $name. Available: [${group.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def nameIndexLens(groupElement: Group, name: String, ix: Int): Either[DocumentError, (Group, SGLens)] =
    groupElement.fields.find(_.label === name) match {
      case Some(ge @ Group(_, _, _)) =>
        val lens = new DocumentLens[SingleGroupValue, GroupValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, GroupValue] = {
            p.getElement(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: GroupValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, a: GroupValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, a)
        }
        val composed = Lens.compose(lens, indexLens(ge, ix))
        (ge, SGLens(composed)).asRight
      case Some(_ @Field(_, _)) =>
        IndexError(s"""Field $name can't have index""").asLeft
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def indexLens(element: Group, ix: Int): DocumentLens[GroupValue, SingleGroupValue] =
    new DocumentLens[GroupValue, SingleGroupValue] {

      override def get(p: GroupValue): Either[DocumentError, SingleGroupValue] = {
        if (ix < p.singleGroups.length) p.singleGroups(ix).asRight
        else IndexError(s"Can't get at index $ix with multiplicity ${element.multiplicity}").asLeft
      }

      override def set(groupValue: GroupValue, sa: SingleGroupValue): Either[DocumentError, GroupValue] = {
        val before = groupValue.singleGroups.take(ix).toVector
        val after = groupValue.singleGroups.drop(ix + 1).toVector
        GroupValue((before :+ sa) ++ after).asRight
      }
    }

  def lensFor(group: Group, path: Path): Either[DocumentError, GFLens] = {
    case class Acc(templateElement: TemplateElement, gfLens: Either[DocumentError, GFLens])

    def foldIndexedPathElements(els: Seq[Indexed]): (Group, Either[DocumentError, GFLens]) =
      els.foldLeft((group, GFLens.empty.asRight: Either[DocumentError, GFLens])) {
        // if error, shortcut
        case (acc @ (_, Left(_)), _) => acc
        // First element, group
        case ((group: Group, Right(gfLens)), indexed) =>
          logger.debug(s"Enter lens construction iteration at element: ${group.label}, path: $indexed")
          nameIndexLens(group, indexed.name, indexed.index) match {
            case Right((group, glens)) =>
              logger.debug(s"outcome: Group, nextTemplate: ${group.label} lens: ${glens}")
              (group, gfLens.append(glens))
            case Left(e) =>
              logger.debug(s"Left Outcome: $e")
              (group, Left(e))
          }
      }

    path match {
      case Path(indexed, Some(Named(name))) =>
        foldIndexedPathElements(indexed) match {
          case (_, Left(e)) => Left(e)
          case (group, Right(gflens)) =>
            nameLens(group, name).flatMap {
              case (_, flens) =>
                gflens.append(flens)
            }
        }
      case Path(indexed, None) =>
        foldIndexedPathElements(indexed) match {
          case (_, Left(e)) => Left(e)
          case (_, Right(gflens)) => gflens.asRight
        }
    }
  }

  def fieldLensFor(groupElement: Group, path: Path): Either[DocumentError, DocumentLens[SingleGroupValue, FieldValue]] =
    lensFor(groupElement, path).flatMap(_.asFieldLens)

  def singleGroupLensFor(
    groupElement: Group,
    path: Path): Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
    lensFor(groupElement, path).flatMap(_.asSingleGroupLens)

  def groupLensFor(groupElement: Group, path: Path): Either[DocumentError, DocumentLens[SingleGroupValue, GroupValue]] =
    lensFor(groupElement, path).flatMap(_.asGroupLens)
}
object DocumentLenses extends DocumentLenses
