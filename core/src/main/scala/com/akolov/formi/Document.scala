package com.akolov.formi
import cats.implicits._
import com.akolov.formi.errors.DocumentError
import com.akolov.formi.errors._
import com.akolov.formi.lenses.Lens
import org.log4s.getLogger

case class Document(templateElement: TemplateElement[SValue], innerValue: InputValue)

case class Indexed(name: String, index: Int = 0)
case class Path(path: List[Indexed])

object Path {
  def apply(els: Indexed*) = new Path(els.toList)
}

object DocumentOps {
  def fromTemplate(te: TemplateElement[SValue]) = Document(te, te.emptySingle)
}

trait DocumentLens {
  val logger = getLogger

  type DocumentLens[P, A] = Lens[P, DocumentError, A]

  trait SingleGroupLens

  object SingleGroupLens {
    case class FLens(lens: DocumentLens[SingleGroupValue, SingleFieldValue]) extends SingleGroupLens
    case class GLens(lens: DocumentLens[SingleGroupValue, SingleGroupValue]) extends SingleGroupLens
  }

  trait SingleToMultiGroupLens

  object SingleToMultiGroupLens {
    case class FLens(lens: DocumentLens[SingleGroupValue, MultiFieldValue]) extends SingleToMultiGroupLens
    case class GLens(lens: DocumentLens[SingleGroupValue, MultiGroupValue]) extends SingleToMultiGroupLens
  }

  case class GFLens(gLens: Option[SingleGroupLens.GLens], fLens: Option[SingleGroupLens.FLens]) { self =>

    /*
    Append a new lens at the end.
     */
    def append(singleGroupLens: SingleGroupLens): Either[DocumentError, GFLens] = singleGroupLens match {
      case gl @ SingleGroupLens.GLens(g) =>
        self.fLens match {
          case None =>
            GFLens(gLens.map(gl => SingleGroupLens.GLens(Lens.compose(gl.lens, g))).orElse(Some(gl)), self.fLens).asRight
          case Some(_) => AttemptTOAppendAfterFLens().asLeft
        }
      case f @ SingleGroupLens.FLens(_) =>
        self.fLens match {
          case None => GFLens(self.gLens, Some(f)).asRight
          case Some(_) => AttemptTOAppendAfterFLens().asLeft
        }
    }

    def asFieldLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleFieldValue]] = (gLens, fLens) match {
      case (Some(SingleGroupLens.GLens(g)), Some(SingleGroupLens.FLens(f))) => Lens.compose(g, f).asRight
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

  def nameLens(groupElement: Group, name: String): Either[DocumentError, (Element, SingleToMultiGroupLens)] =
    groupElement.fields.find(_.label === name) match {
      case Some(ge @ Group(_, _, _)) =>
        val lens = new DocumentLens[SingleGroupValue, MultiGroupValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, MultiGroupValue] = {
            p.get(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: MultiGroupValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, a: MultiGroupValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, a)
        }
        (ge, SingleToMultiGroupLens.GLens(lens)).asRight
      case Some(fe @ Field(_, _, m)) =>
        val lens = new DocumentLens[SingleGroupValue, MultiFieldValue] {
          override def get(p: SingleGroupValue): Either[DocumentError, MultiFieldValue] = {
            p.get(name) match {
              case Left(e) => e.asLeft
              case Right(mgv: MultiFieldValue) => mgv.asRight
              case _ => BadValue().asLeft
            }
          }

          override def set(p: SingleGroupValue, a: MultiFieldValue): Either[DocumentError, SingleGroupValue] =
            p.update(name, a)
        }

        (fe, SingleToMultiGroupLens.FLens(lens)).asRight
      case None =>
        IndexError(s"""No such name: $name. Available: [${groupElement.fields.map(_.label).mkString(",")}]""").asLeft
    }

  def toMultiField(x: MultiElementValue[SingleFieldValue]): MultiFieldValue = MultiFieldValue(x.values)

  def toMultiGroup(x: MultiElementValue[SingleGroupValue]): MultiGroupValue = MultiGroupValue(x.values)

  def nameIndexLens(groupElement: Group, indexed: Indexed): Either[DocumentError, (Element, SingleGroupLens)] = {
    nameLens(groupElement, indexed.name) match {
      case Right((fieldElement: Field, lens: SingleToMultiGroupLens.FLens)) =>
        val fLens = SingleGroupLens.FLens(
          Lens.compose(lens.lens, indexLens(fieldElement, indexed.index).map[MultiFieldValue](a => a)(toMultiField)))
        (fieldElement, fLens).asRight
      case Right((groupElement: Group, lens: SingleToMultiGroupLens.GLens)) =>
        val glens = SingleGroupLens.GLens(
          Lens.compose(lens.lens, indexLens(groupElement, indexed.index).map[MultiGroupValue](a => a)(toMultiGroup)))
        (groupElement, glens).asRight
      case Left(e) => e.asLeft
    }
  }

  def indexLens[SV <: SValue](element: TemplateElement[SV], ix: Int): DocumentLens[MultiElementValue[SV], SV] =
    new DocumentLens[MultiElementValue[SV], SV] {

      override def get(p: MultiElementValue[SV]): Either[DocumentError, SV] = {
        if (ix < p.values.length) p.values(ix).asRight
        else IndexError(s"Element index out of bounds: $ix from ${p.values.length}").asLeft
      }

      override def set(p: MultiElementValue[SV], a: SV): Either[DocumentError, MultiElementValue[SV]] =
        if (ix < p.values.length)
          MultiElementValue((p.values.take(ix) :+ a) ++ p.values.drop(ix + 1)).asRight
        else if (ix == p.values.length && element.multiplicity.isUnderMax(ix)) {
          // insert the first available slot
          MultiElementValue(p.values ++ Vector.fill(ix - p.values.length)(element.emptySingle)).asRight
        } else
          IndexError(s"Can't set at index 2 $ix with multiplicity ${element.multiplicity}").asLeft
    }

  def fieldIndexLens(fieldElement: Field, ix: Int): DocumentLens[MultiFieldValue, SingleFieldValue] = {
    indexLens[SingleFieldValue](fieldElement, ix)
      .map[MultiFieldValue](identity)(toMultiField)
  }

  def groupIndexLens(ge: Group, ix: Int): DocumentLens[MultiGroupValue, SingleGroupValue] =
    indexLens[SingleGroupValue](ge, ix)
      .map[MultiGroupValue](identity)(toMultiGroup)

  def lensFor(groupElement: Group, path: Path): Either[DocumentError, GFLens] = {
    case class Acc(templateElement: TemplateElement[SValue], lens: Either[DocumentError, GFLens])

    val acc = path.path.foldLeft(Acc(groupElement, GFLens.empty.asRight)) {
      // if error, shortcut
      case (acc @ Acc(_, Left(_)), _) => acc
      // Firs element, group
      case (Acc(ge @ Group(l, fs, m), Right(gfLens)), indexed) =>
        logger.debug(s"Enter lens calculation iteration at element: ${ge.label}, path: $indexed")
        nameIndexLens(ge, indexed) match {
          case Right((fieldTemplate: Field, flens: SingleGroupLens.FLens)) =>
            logger.debug(s"outcome: Field, nextTemplate: ${fieldTemplate.label} lens: ${flens}")
            Acc(fieldTemplate, gfLens.append(flens))
          case Right((groupTemplate: Group, glens: SingleGroupLens.GLens)) =>
            logger.debug(s"outcome: Group, nextTemplate: ${groupTemplate.label} lens: ${glens}")
            logger.debug(s"nextLens:  ${gfLens.append(glens)}")
            Acc(groupTemplate, gfLens.append(glens))
          case Left(e) =>
            logger.debug(s"LeftOutcome")
            Acc(groupElement, Left(e))
        }
    }
    acc.lens
  }

  def fieldLensFor(groupElement: Group, path: Path) =
    lensFor(groupElement, path).flatMap(_.asFieldLens)
}
object DocumentLens extends DocumentLens
