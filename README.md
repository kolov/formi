# Formi

Document schema and manipulation with functional lenses

# Usage

Create a template, use it to create an empty document from it:

```scala

import com.akolov.forms._

val template = Template( 
  "Simple CV",
  GroupElement(
    "content",
    List(
      GroupElement(
        "Head",
        fields = List(
          FieldElement(label = "name", desc = Text(maxLength = Some(50), pattern = None), multiplicity= Multiplicity.Once),
           FieldElement(label = "title", desc = Text(Some(50)))
        )), 
      GroupElement(
        "Links",
        fields = List(
          GroupElement(
              "Link",
              fields = List(
                FieldElement(label = "linkName", desc = Text(Some(12))),
                FieldElement(label = "linkValue", desc = Text(Some(25)))
              ),
              multiplicity = Multiplicity.AtLeastOnce
            )
          )
        )
      )
    )
  )

val document = template.empty
```
 
The document content can be access through lenses. Because of the dynamic document structure, lens creation may fail.

```scala
import com.akolov.forms.DocumentLens._


for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name
// res0: Either[errors.DocumentError, SingleFieldValue] = Right(
//   SingleFieldValue(None)
// )
```

Any field can be set and queried: 

```scala
for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
  updated <- fieldLens.set(document, SingleFieldValue("George Costanza"))
  name <- fieldLens.get(updated)
} yield name
// res1: Either[errors.DocumentError, SingleFieldValue] = Right(
//   SingleFieldValue(Some("George Costanza"))
// )
```

Setting and reading may fail because of the schema multiplicity:

```scala
for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 1),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name
// res2: Either[errors.DocumentError, SingleFieldValue] = Left(
//   IndexError("Element index out of bounds: 1 from 1")
// )
```
## Developer's notes

    sbt '+ publishSigned'
    sbt sonatypeReleaseAll

    sbt  docs/mdoc // project-docs/target/mdoc/README.md
 


 


