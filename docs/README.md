# Formi

Document schema and manipulation with functional lenses

# Usage

Create a template, use it to create an empty document from it:

```scala mdoc:silent

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

```scala mdoc
import com.akolov.forms.DocumentLens._


for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name

```

Any field can be set and queried: 

```scala mdoc

for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
  updated <- fieldLens.set(document, SingleFieldValue("George Costanza"))
  name <- fieldLens.get(updated)
} yield name

```

Setting and reading may fail because of the schema multiplicity:

```scala mdoc
for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 1),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name
```
## Developer's notes

    sbt '+ publishSigned'
    sbt sonatypeReleaseAll

    sbt  docs/mdoc // project-docs/target/mdoc/README.md
 


 


