# `type.WHO<k> := v` parses in expression position

A bind whose left side is a subscript on a method-call result
(`my \symbol = (type.WHO<ResultSeq> := Proxy.new(...))`) was a parse error in
expression position, though the statement form worked. Red 0.2.8's
`MetamodelX::Red::Model` uses exactly that shape, so `use Red` could not
compile. The expression-context indexed bind now accepts a method-call
result as its target, like the statement form, and `use Red; model Bla { }`
loads and answers like rakudo.
