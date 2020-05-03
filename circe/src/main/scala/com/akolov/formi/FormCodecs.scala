package com.akolov.formi

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

trait FormCodecs {
  implicit val encoderMultiplicity: Encoder[Multiplicity] = deriveEncoder
  implicit val decoderMultiplicity: Decoder[Multiplicity] = deriveDecoder

  implicit val encoderTextField: Encoder[Text] = deriveEncoder
  implicit val decoderTextField: Decoder[Text] = deriveDecoder

  implicit val encoderDateField: Encoder[Date] = deriveEncoder
  implicit val decoderDateField: Decoder[Date] = deriveDecoder

  implicit val encoderInputParameters: Encoder[InputDesc] = deriveEncoder
  implicit val decoderInputParameters: Decoder[InputDesc] = deriveDecoder

  implicit val encoderEntry: Encoder[TemplateElement] = deriveEncoder
  implicit val decoderEntry: Decoder[TemplateElement] = deriveDecoder

  implicit lazy val SingleValueEncoder = deriveEncoder[FieldValue]
  implicit lazy val SingleValueeDecoder = deriveDecoder[FieldValue]

  implicit lazy val GroupValueEncoder: Encoder[GroupValue] = deriveEncoder[GroupValue]
  implicit lazy val GroupValueValueecoder: Decoder[GroupValue] = deriveDecoder[GroupValue]

  implicit lazy val ValueEncoder: Encoder[Value] = deriveEncoder[Value]
  implicit lazy val Valuecoder: Decoder[Value] = deriveDecoder[Value]

  implicit lazy val SingleGroupValueEncoder: Encoder[SingleGroupValue] = deriveEncoder[SingleGroupValue]
  implicit lazy val SingleGroupValueecoder: Decoder[SingleGroupValue] = deriveDecoder[SingleGroupValue]

  implicit lazy val FieldElementEncoder: Encoder[Field] = deriveEncoder[Field]
  implicit lazy val FieldElementDecoder: Decoder[Field] = deriveDecoder[Field]

  implicit lazy val groupElementEncoder: Encoder[Group] = deriveEncoder[Group]
  implicit lazy val GroupElementDecoder: Decoder[Group] = deriveDecoder[Group]

  implicit val encoderDocumentDefinition: Encoder[Template] = deriveEncoder
  implicit val decoderDocumentDefinition: Decoder[Template] = deriveDecoder

  implicit lazy val FieldFormEntryEncoder: Encoder[FieldEntry] = deriveEncoder[FieldEntry]
  implicit lazy val FieldFormEntryDecoder: Decoder[FieldEntry] = deriveDecoder[FieldEntry]
  implicit lazy val GroupFormEntryEncoder: Encoder[GroupEntry] = deriveEncoder[GroupEntry]
  implicit lazy val GroupFormEntryDecoder: Decoder[GroupEntry] = deriveDecoder[GroupEntry]
  implicit lazy val FormEntryEncoder: Encoder[Entry] = deriveEncoder[Entry]
  implicit lazy val FormEntryDecoder: Decoder[Entry] = deriveDecoder[Entry]
  implicit lazy val SingleGroupFormEntryEncoder: Encoder[SingleGroupFormEntry] = deriveEncoder[SingleGroupFormEntry]
  implicit lazy val SingleGroupFormEntryDecoder: Decoder[SingleGroupFormEntry] = deriveDecoder[SingleGroupFormEntry]
}
