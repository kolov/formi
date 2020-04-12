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

  implicit val encoderEntry: Encoder[TemplateElement[SingleGroupValue]] = deriveEncoder
  implicit val decoderEntry: Decoder[TemplateElement[SingleGroupValue]] = deriveDecoder

  implicit lazy val SingleValueEncoder = deriveEncoder[SingleFieldValue]
  implicit lazy val SingleValueeDecoder = deriveDecoder[SingleFieldValue]

  implicit lazy val ContainedValuencoder: Encoder[InputValue] = ??? // deriveEncoder[ElementValue]
  implicit lazy val ContainedValueDecoder: Decoder[InputValue] = ??? // deriveDecoder[ElementValue]

  implicit lazy val SingleGroupValueEncoder: Encoder[SingleGroupValue] = ??? //deriveEncoder[SingleGroupValue]
  implicit lazy val SingleGroupValueecoder: Decoder[SingleGroupValue] = ??? //deriveDecoder[SingleGroupValue]
  23
  implicit lazy val FieldElementEncoder: Encoder[Field] = deriveEncoder[Field]
  implicit lazy val FieldElementDecoder: Decoder[Field] = deriveDecoder[Field]

  implicit lazy val dsdsfdsf: Encoder[TemplateElement[SingleFieldValue]] =
    deriveEncoder[TemplateElement[SingleFieldValue]]

  implicit lazy val dfsdsgre: Decoder[TemplateElement[SingleFieldValue]] =
    deriveDecoder[TemplateElement[SingleFieldValue]]

  implicit lazy val dds: Encoder[TemplateElement[SingleGroupValue]] = deriveEncoder[TemplateElement[SingleGroupValue]]
  implicit lazy val dfdf: Decoder[TemplateElement[SingleGroupValue]] = deriveDecoder[TemplateElement[SingleGroupValue]]

  implicit lazy val sadsads: Encoder[TemplateElement[SValue]] =
    deriveEncoder[TemplateElement[SValue]]

  implicit lazy val sadsadsad: Decoder[TemplateElement[SValue]] =
    deriveDecoder[TemplateElement[SValue]]

  implicit lazy val roupElementEncoder: Encoder[Group] = deriveEncoder[Group]
  implicit lazy val GroupElementDecoder: Decoder[Group] = deriveDecoder[Group]

  implicit val encoderDocumentDefinition: Encoder[Template] = deriveEncoder
  implicit val decoderDocumentDefinition: Decoder[Template] = deriveDecoder
}
