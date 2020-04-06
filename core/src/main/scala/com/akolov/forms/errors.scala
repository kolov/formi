package com.akolov.forms

package errors

sealed trait DocumentError
case class NotGroupPath(desc: String = "") extends DocumentError
case class NotFieldPath(desc: String) extends DocumentError
case class PathError(desc: String = "") extends DocumentError
case class IndexError(desc: String) extends DocumentError
case class InconsistentDocument(desc: String) extends DocumentError
case class AttemptTOAppendAfterFLens(desc: String = "") extends DocumentError
case class BadValue(desc: String = "") extends DocumentError
