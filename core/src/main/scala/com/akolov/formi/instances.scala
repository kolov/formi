import cats.Show
import com.akolov.formi.{MultiFieldValue, SingleGroupValue}

trait ShowInstances {

  implicit val showMultiFieldValue = new Show[MultiFieldValue] {

    override def show(t: MultiFieldValue): String =
      s"""MFV( ${t.values.mkString(",")})""".stripMargin
  }

  implicit val showSingleGroupValue = new Show[SingleGroupValue] {

    override def show(t: SingleGroupValue): String =
      s""" SGV( ${t.values.map { case (k, v) => s"$k==$v" }.mkString(",")})""".stripMargin
  }
}

object ShowInstances extends ShowInstances
