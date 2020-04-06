# Formi

Describe document structure in a template. Create, manipulate and query documents following this structure. 

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
// template: Template = Template(
//   "Simple CV",
//   GroupElement(
//     "content",
//     List(
//       GroupElement(
//         "Head",
//         List(
//           FieldElement("name", Text(Some(50), None), Multiplicity(1, Some(1))),
//           FieldElement("title", Text(Some(50), None), Multiplicity(1, Some(1)))
//         ),
//         Multiplicity(1, Some(1))
//       ),
//       GroupElement(
//         "Links",
//         List(
//           GroupElement(
//             "Link",
//             List(
//               FieldElement(
//                 "linkName",
//                 Text(Some(12), None),
//                 Multiplicity(1, Some(1))
//               ),
//               FieldElement(
//                 "linkValue",
//                 Text(Some(25), None),
//                 Multiplicity(1, Some(1))
//               )
//             ),
//             Multiplicity(1, None)
//           )
//         ),
//         Multiplicity(1, Some(1))
//       )
//     ),
//     Multiplicity(1, Some(1))
//   )
// )

val document = template.empty
// document: SingleGroupValue = SingleGroupValue(
//   List(
//     (
//       "Head",
//       MultiGroupValue(
//         List(
//           SingleGroupValue(
//             List(
//               ("name", MultiFieldValue(List(SingleFieldValue(None)))),
//               ("title", MultiFieldValue(List(SingleFieldValue(None))))
//             )
//           )
//         )
//       )
//     ),
//     (
//       "Links",
//       MultiGroupValue(
//         List(
//           SingleGroupValue(
//             List(
//               (
//                 "Link",
//                 MultiGroupValue(
//                   List(
//                     SingleGroupValue(
//                       List(
//                         (
//                           "linkName",
//                           MultiFieldValue(List(SingleFieldValue(None)))
//                         ),
//                         (
//                           "linkValue",
//                           MultiFieldValue(List(SingleFieldValue(None)))
//                         )
//                       )
//                     )
//                   )
//                 )
//               )
//             )
//           )
//         )
//       )
//     )
//   )
// )
```
 
The document content can be access through lenses:1  

```scala
import com.akolov.forms.DocumentLens._

val lensName = lensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
// lensName: Either[errors.DocumentError, GFLens] = Right(
//   GFLens(
//     Some(GLens(com.akolov.forms.lenses.Lens$$anon$2@3f08249d)),
//     Some(FLens(com.akolov.forms.lenses.Lens$$anon$2@3528de8c))
//   )
// )

for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 0),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name
// res0: Either[errors.DocumentError, SingleFieldValue] = Right(
//   SingleFieldValue(None)
// )
```

Name can be set: 

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

The lens takes into account the multiplicity:

```scala
for {
  fieldLens <- fieldLensFor(template.body, Path( Indexed( "Head", 1),  Indexed( "name", 0)))
  name <- fieldLens.get(document)
} yield name
// res2: Either[errors.DocumentError, SingleFieldValue] = Left(
//   IndexError("Can't read: 1 from 1")
// )
```
## Developer's notes

    sbt '+ publishSigned'
    sbt sonatypeReleaseAll

    sbt  docs/mdoc // project-docs/target/mdoc/README.md
 


 


