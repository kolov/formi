# Formi

Document schema-based manipulation with functional lenses

# Usage

Add the dependency

     "com.akolov" %% "formi-circe" %"0.2.1"

A template describes the structure of a document.
From a template, an empty document can be created:

```scala mdoc:silent

import com.akolov.formi._

val template = Template( 
  "Simple CV",
  Group(
    "content",
    List(
      Group(
        "Head",
        fields = List(
          Field(label = "name", input = Text(maxLength = Some(50), pattern = None)),
           Field(label = "title", input = Text(Some(50)))
        )), 
      Group(
        "Links",
        fields = List(
          Group(
              "Link",
              fields = List(
                Field(label = "linkName", input = Text(Some(12))),
                Field(label = "linkValue", input = Text(Some(25)))
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
import com.akolov.formi.lenses._
import com.akolov.formi.lenses.DocumentLenses._
import com.akolov.formi.SingleGroupValueOps._

for {
  path <- Path.parsePath("Head[0]/name")
  fieldLens <- fieldLensFor(template.body, path)
  name <- fieldLens.get(document)
} yield name

```

Any field can be set and queried: 

```scala mdoc

for {
  path <- Path.parsePath("Head[0]/name")
  fieldLens <- fieldLensFor(template.body, path)
  updated <- fieldLens.set(document, FieldValue("George Costanza"))
  name <- fieldLens.get(updated)
} yield name

```

Groups instances can be added or removed, if the group multiplicity allows. This 
will insert a new Link group at position 0

```scala mdoc

document.insertAt(template.body, "Links[0]/Link", 0)
 
```
## Developer's notes

    sbt publishSigned
    sbt sonatypeReleaseAll

    sbt  docs/mdoc // project-docs/target/mdoc/README.md
 


 


