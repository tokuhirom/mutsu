# A lexical class exported through `OUR::<DEFAULT>` keeps its type object

`OUR::<Foo> := Foo` (a `my class`) in a module, published with
`my package EXPORT { OUR::<DEFAULT> := Module }`, imports as the class. The shape
already worked on `main` (an earlier fix landed it without a test); a regression
test now pins it (#15258).
