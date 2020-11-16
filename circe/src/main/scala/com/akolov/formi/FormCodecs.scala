package com.akolov.formi

import com.akolov.formi.compact.CompactTemplate
import com.akolov.formi.compact.CompactTemplate.CompactTemplateElement
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

trait FormCodecs {
  implicit val encoderMultiplicity: Encoder[Multiplicity] = deriveEncoder
  implicit val decoderMultiplicity: Decoder[Multiplicity] = deriveDecoder

  implicit val encoderInputDesc: Encoder[InputDesc] = deriveEncoder
  implicit val decoderInputDesc: Decoder[InputDesc] = deriveDecoder

  implicit val encoderTemplateElement: Encoder[TemplateElement] = deriveEncoder
  implicit val decoderTemplateElement: Decoder[TemplateElement] = deriveDecoder

  implicit lazy val encoderFieldValue = deriveEncoder[FieldValue]
  implicit lazy val decoderFieldValue = deriveDecoder[FieldValue]

  implicit lazy val GroupValueEncoder: Encoder[GroupValue] = deriveEncoder[GroupValue]
  implicit lazy val GroupValueValueecoder: Decoder[GroupValue] = deriveDecoder[GroupValue]

  implicit lazy val ValueEncoder: Encoder[Value] = deriveEncoder[Value]
  implicit lazy val Valuecoder: Decoder[Value] = deriveDecoder[Value]

  implicit lazy val SingleGroupValueEncoder: Encoder[SingleGroupValue] = deriveEncoder[SingleGroupValue]
  implicit lazy val SingleGroupValueecoder: Decoder[SingleGroupValue] = deriveDecoder[SingleGroupValue]

  implicit lazy val encoderField: Encoder[Field] = deriveEncoder[Field]
  implicit lazy val decoderField: Decoder[Field] = deriveDecoder[Field]

  implicit lazy val encoderGroup: Encoder[Group] = deriveEncoder[Group]
  implicit lazy val decoderGroup: Decoder[Group] = deriveDecoder[Group]

  // Compact template
  implicit val encoderCompactTemplateElement: Encoder[CompactTemplateElement] = deriveEncoder
  implicit val decoderCompactTemplateElement: Decoder[CompactTemplateElement] = deriveDecoder

  implicit val encoderDocumentDefinition: Encoder[Template] = deriveEncoder
  implicit val decoderDocumentDefinition: Decoder[Template] = deriveDecoder

  implicit lazy val FieldFormEntryEncoder: Encoder[FieldEntry] = deriveEncoder[FieldEntry]
  implicit lazy val FieldFormEntryDecoder: Decoder[FieldEntry] = deriveDecoder[FieldEntry]
  implicit lazy val GroupFormEntryEncoder: Encoder[GroupEntry] = deriveEncoder[GroupEntry]
  implicit lazy val GroupFormEntryDecoder: Decoder[GroupEntry] = deriveDecoder[GroupEntry]
  implicit lazy val FormEntryEncoder: Encoder[Entry] = deriveEncoder[Entry]
  implicit lazy val FormEntryDecoder: Decoder[Entry] = deriveDecoder[Entry]
}
