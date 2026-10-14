# RakuAST: indirect type captures

`::(q<Foo>) $x` now renders as rakudo does: `Parameter.type-captures` with a
`Type::Capture` whose `Name` is one `Name::Part::Expression`. The converter
reparses the capture's expression text; lowering spells a plain string
expression back as the parser's `::('Foo')` constraint. One more `t/` file
passes under `MUTSU_RAKUAST=1` (ADR-10723 S10). A non-literal expression stays
the boundary on the lowering side.
