package com.akolov.formi

package lenses
import cats.implicits._
import com.akolov.formi.errors.{DocumentError, NotGroupPath}
import com.akolov.formi.lenses.DocumentLenses._

package object syntax {

  implicit class GroupOps(group: Group) {

    def getSubGroup(path: Path): Either[DocumentError, Group] = {
      val els: Seq[String] = path.groups.map(_.name) ++ path.named.map(_.name).toList
      els
        .foldLeft[Option[Group]](Some(group)) {
          case (Some(g), name) =>
            g.fields.find(_.label == name) match {
              case Some(ng @ Group(_, _, _)) => Some(ng)
              case _ => None
            }
        }
        .fold(NotGroupPath(path.toString).asInstanceOf[DocumentError].asLeft[Group])(_.asRight[DocumentError])
    }
  }

  implicit class SingleGroupOps(sg: SingleGroupValue) {

    def updateField(pathString: String, group: Group, fv: FieldValue): Either[DocumentError, SingleGroupValue] =
      for {
        path <- Path.parsePath(pathString)
        r <- updateField(path, group, fv)
      } yield r

    def updateField(path: Path, group: Group, fv: FieldValue): Either[DocumentError, SingleGroupValue] =
      for {
        lens <- fieldLensFor(group, path)
        r <- lens.set(sg, fv)
      } yield r

    def getField(pathString: String, group: Group): Either[DocumentError, FieldValue] =
      for {
        path <- Path.parsePath(pathString)
        r <- getField(path, group)
      } yield r

    def getField(path: Path, group: Group): Either[DocumentError, FieldValue] =
      for {
        lens <- fieldLensFor(group, path)
        r <- lens.get(sg)
      } yield r
  }
}
