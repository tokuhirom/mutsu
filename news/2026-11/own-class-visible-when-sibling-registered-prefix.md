# A compunit sees the class it declares itself, even when a sibling owns the package prefix

`use Mathematica::Serializer; use Mathematica::Deserializer;` died with `Could not find symbol
'Mathematica::Deserializer::WXF::Symbol'` while `Deserializer/WXF.rakumod` ran its own
`our constant S = Mathematica::Deserializer::WXF::Symbol`: the qualified-name visibility gate
matched the shortest registered prefix `Mathematica` (owned by the sibling loaded first) instead of
the unit that declares the class. A class a unit declares itself is now visible to that unit.
Mathematica::Serializer's three test files now match raku (3/13/35).
