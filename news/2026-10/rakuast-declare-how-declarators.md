# EXPORTHOW::DECLARE declarators cross the RakuAST boundary

A module can register a new class-like keyword through
`EXPORTHOW::DECLARE` — OO::Monitors' `monitor` is the best-known one.
Under `MUTSU_RAKUAST=1` every such declaration stopped with a
`__mutsu_declare_how` refusal, because the converter had no RakuAST shape
for the parser's marker.

Measured on rakudo 2026.09, `monitor Counter { ... }` is the ordinary
`RakuAST::Class` with one more field, `parsed-declarator => "monitor"`
(`RakuAST::Role` when the HOW is a role HOW). The resolved metaobject is
not part of the rendered tree, so the keyword the parser already records is
all the round trip needs. The converter now renders the field, lowering
turns it back into the marker, and the class gets the module's HOW when it
is registered, as a parsed declaration does. `.parsed-declarator` answers
`"class"`, `"role"` or `"grammar"` for the built-in declarators.

The marker name is now one constant (`parser::DECLARE_HOW_TRAIT`) with a
builder and a reader, in place of five copies of the string in the parser.
