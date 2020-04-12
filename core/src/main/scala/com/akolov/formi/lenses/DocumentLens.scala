package com.akolov.formi.lenses

import cats.implicits._
import com.akolov.formi._
import com.akolov.formi.errors.{DocumentError, _}
import org.log4s.getLogger

case class Document(templateElement: TemplateElement, value: Value)

trait PathElement
case class Indexed(name: String, index: Int = 0) extends PathElement
case class Named(name: String) extends PathElement
case class Path(groups: List[Indexed], named: Named)

object DocumentOps {
  def fromTemplate(te: TemplateElement) = Document(te, te.empty)
}

trait DocumentLens {
  val logger = getLogger

  type DocumentLens[P, A] = Lens[P, DocumentError, A]

  sealed trait XxxLens

  case class GLens(lens: DocumentLens[SingleGroupValue, SingleGroupValue]) extends XxxLens

  case class FLens(lens: DocumentLens[SingleGroupValue, FieldValue]) extends XxxLens

  case class GFLens(gLens: Option[GLens], fLens: Option[FLens]) { self =>

    /*
    Append a new lens at the end.
     */
    def append(l: XxxLens): Either[DocumentError, GFLens] = l match {
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

    def asGroupLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] = (gLens, fLens) match {
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

    override def set(p: P, a: A): Either[DocumentError, P] = Left(e)
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

          override def set(p: SingleGroupValue, a: FieldValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, a)
        }
        (fe, FLens(lens)).asRight
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

//  def toMultiField(x: MultiElementValue[FieldValue]): MultiFieldValue = MultiFieldValue(x.values)

  def toMultiGroup(x: GroupValue): GroupValue = GroupValue(x.values)

  def nameIndexLens(groupElement: Group, name: String, ix: Int): Either[DocumentError, (Element, GLens)] =
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

          override def set(p: SingleGroupValue, a: GroupValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, a)
        }
        val composed = Lens.compose(lens, indexLens(ge, ix))
        (ge, GLens(composed)).asRight
      case Some(_ @Field(_, _)) =>
        IndexError(s"""Field $name can't have index""").asLeft
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def indexLens(element: Group, ix: Int): DocumentLens[GroupValue, SingleGroupValue] =
    new DocumentLens[GroupValue, SingleGroupValue] {

      override def get(p: GroupValue): Either[DocumentError, SingleGroupValue] = {
        if (ix < p.values.length) p.values(ix).asRight
        else IndexError(s"Can't get at index $ix with multiplicity ${element.multiplicity}").asLeft
      }

      override def set(p: GroupValue, a: SingleGroupValue): Either[DocumentError, GroupValue] =
        if (ix < p.values.length) {
          GroupValue((p.values.take(ix).toVector :+ a) ++ p.values.drop(ix + 1)).asRight
        }
        else if (ix == p.values.length && element.multiplicity.isUnderMax(ix)) {
          // insert the first available slot only
          GroupValue(p.values ++ Vector.fill(ix - p.values.length)(element.singleEmpty)).asRight
        } else
          //error beyond the first slot
          IndexError(s"Can't set at index $ix with multiplicity ${element.multiplicity}").asLeft
    }

  def lensFor(groupElement: Group, path: Path): Either[DocumentError, GFLens] = {
    case class Acc(templateElement: TemplateElement, lens: Either[DocumentError, GFLens])

    val acc: (TemplateElement, Either[DocumentError, Option[GLens]]) =
      path.groups.foldLeft((groupElement: TemplateElement, None.asRight: Either[DocumentError, Option[GLens]])) {
        // if error, shortcut
        case (acc @ (_, Left(_)), _) => acc
        // Firs element, group
        case ((ge @ Group(l, fs, m), Right(otLens)), indexed) =>
          logger.debug(s"Enter lens calculation iteration at element: ${ge.label}, path: $indexed")
          nameIndexLens(ge, indexed.name, indexed.index) match {
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
        case ((fe @ Field(_, _), Left(_)), index) =>
          (fe, PathError(s"Group element expected for ${index}").asLeft)
      }
    acc match {
      case (_, Left(e)) => Left(e)
      case (ge @ Group(l, fs, m), Right(glens)) =>
        nameLens(ge, path.named.name).map {
          case (_, flens) =>
            GFLens(glens, Some(flens))
        }

      case (fe @ Field(l, m), _) => PathError(" Index incomplete").asLeft
    }
  }

  def fieldLensFor(groupElement: Group, path: Path) =
    lensFor(groupElement, path).flatMap(_.asFieldLens)
}
object DocumentLens extends DocumentLens
