# Formi

Document schema and manipulation with functional lenses

# Usage

Create a template, use it to create an empty document from it:

```scala

import com.akolov.formi._

val template = Template( 
  "Simple CV",
  Group(
    "content",
    List(
      Group(
        "Head",
        fields = List(
          Field(label = "name", desc = Text(maxLength = Some(50), pattern = None)),
           Field(label = "title", desc = Text(Some(50)))
        )), 
      Group(
        "Links",
        fields = List(
          Group(
              "Link",
              fields = List(
                Field(label = "linkName", desc = Text(Some(12))),
                Field(label = "linkValue", desc = Text(Some(25)))
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
import com.akolov.formi.lenses.DocumentLenses._


for {
  fieldLens <- fieldLensFor(template.body,Path(  "Head", 0, "name"))
  name <- fieldLens.get(document)
} yield name
// res0: Either[errors.DocumentError, FieldValue] = Right(FieldValue(None))
```

Any field can be set and queried: 

```scala
for {
  fieldLens <- fieldLensFor(template.body, Path(  "Head", 0, "name"))
  updated <- fieldLens.set(document, FieldValue("George Costanza"))
  name <- fieldLens.get(updated)
} yield name
// res1: Either[errors.DocumentError, FieldValue] = Right(
//   FieldValue(Some("George Costanza"))
// )
```

Setting and reading may fail because of the schema multiplicity:

```scala
for {
  fieldLens <- fieldLensFor(template.body,Path(  "Head", 0, "name"))
  name <- fieldLens.get(document)
} yield name
// res2: Either[errors.DocumentError, FieldValue] = Right(FieldValue(None))
```
## Developer's notes

    sbt '+ publishSigned'
    sbt sonatypeReleaseAll

    sbt  docs/mdoc // project-docs/target/mdoc/README.md
 


 


