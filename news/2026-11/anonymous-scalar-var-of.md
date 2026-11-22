# `.VAR` of an anonymous scalar or an item answers `.of`

`(my $ = 5).VAR.of` died with "No such method 'of' for invocant of type
'Scalar'", and so did `$(1, 2).VAR.of`. Their `.VAR` answered the `Scalar`
*type object*, which has no container methods. A typed anonymous scalar
(`(my Int $ = 5).VAR`) went through the method call by its synthesized
variable name instead, which holds only the bare value, so `.VAR` answered
`Int`.

All three now answer a `Scalar` container view, as an element's `.VAR`
does. It reports `.of` as the declared type: `Mu`, or `Int` for
`my Int $` (#18349).
