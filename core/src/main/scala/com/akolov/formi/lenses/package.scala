package com.akolov.formi

package lenses
import cats.implicits._
import com.akolov.formi.errors.{DocumentError, NotGroupPath}
import com.akolov.formi.lenses.DocumentLenses._

package object syntax {

  implicit class GroupOps(group: Group) {

    def getSubGroup(path: Path): Either[DocumentError, Group] = {
      val els: Seq[String] = path.asStrings
      els
        .foldLeft[Option[Group]](Some(group)) {
          case (Some(g), name) =>
            g.fields.find(_.label == name) match {
              case Some(ng @ Group(_, _, _, _)) => Some(ng)
              case _ => None
            }
        }
        .fold(NotGroupPath(path.toString).asInstanceOf[DocumentError].asLeft[Group])(_.asRight[DocumentError])
    }
  }

  implicit class SingleGroupOps(sgv: SingleGroupValue) {

    def updateField(pathString: String, group: Group, fv: FieldValue): Either[DocumentError, SingleGroupValue] =
      for {
        path <- Path.parsePath(pathString)
        r <- updateField(path, group, fv)
      } yield r

    def updateField(path: Path, group: Group, fv: FieldValue): Either[DocumentError, SingleGroupValue] =
      for {
        lens <- fieldLensFor(group, path)
        r <- lens.set(sgv, fv)
      } yield r

    def getField(pathString: String, group: Group): Either[DocumentError, FieldValue] =
      for {
        path <- Path.parsePath(pathString)
        r <- getField(path, group)
      } yield r

    def getGroup(pathString: String, group: Group): Either[DocumentError, SingleGroupValue] =
      for {
        path <- Path.parsePath(pathString)
        lens <- singleGroupLensFor(group, path)
        r <- lens.get(sgv)
      } yield r

    def getField(path: Path, group: Group): Either[DocumentError, FieldValue] =
      for {
        lens <- fieldLensFor(group, path)
        r <- lens.get(sgv)
      } yield r

    def insertAt(group: Group, pathString: String, at: Int): Either[DocumentError, SingleGroupValue] =
      Path.parsePath(pathString).flatMap(path => insertAt(group, path, at))

    def insertAt(group: Group, path: Path, at: Int): Either[DocumentError, SingleGroupValue] =
      for {
        lens <- groupLensFor(group, path)
        currentGroup <- lens.get(sgv)
        subGroup <- group.getSubGroup(path)

        updated = {
          val before = currentGroup.singleGroups.take(at)
          val after = currentGroup.singleGroups.drop(at)

          (before :+ subGroup.singleEmpty) ++ after
        }
        updated <- lens.set(sgv, GroupValue(updated))
      } yield updated

    def deleteAt(group: Group, pathString: String, at: Int): Either[DocumentError, SingleGroupValue] =
      Path.parsePath(pathString).flatMap(path => deleteAt(group, path, at))

    def deleteAt(group: Group, path: Path, at: Int): Either[DocumentError, SingleGroupValue] =
      for {
        lens <- groupLensFor(group, path)
        currentGroup <- lens.get(sgv)
        subGroup <- group.getSubGroup(path)

        updated = {
          val before = currentGroup.singleGroups.take(at)
          val after = currentGroup.singleGroups.drop(at + 1)
          before ++ after
        }
        updated <- lens.set(sgv, GroupValue(updated))
      } yield updated
  }
}
