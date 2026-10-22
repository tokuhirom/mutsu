# The `<declarator>-attr` attribute metaclass of EXPORTHOW::DECLARE

A class declared with an `EXPORTHOW::DECLARE` declarator whose module also exports a
`<declarator>-attr` entry now runs that metaclass's `compose` for each `has` attribute when
the class is declared (ValueClass's `value-class-attr` dies there for an `is rw` attribute).
`.^attributes` still answers plain `Attribute` objects and `container_initializer` is not
consulted yet (#14588).
