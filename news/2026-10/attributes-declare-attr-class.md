# `.^attributes` answers the declarator's `-attr` class

A class declared with an `EXPORTHOW::DECLARE` keyword that also exports
`<keyword>-attr` now reports its `has` attributes through `.^attributes` as
instances of that class, as Rakudo does, instead of plain `Attribute` objects.
