# Imported values preserve qualified name boundaries

An imported value whose name matches the first component of a package name no
longer makes the parser split that qualified name. For example, importing a
`GEOS` value still permits `GEOS::Geometry.new`, including inside method bodies.
The imported term parser now defers a following `::` to the qualified identifier
parser, matching the rule already applied to locally declared terms.
