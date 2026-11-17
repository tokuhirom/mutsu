# RakuAST: `my @arr of Int` keeps its `of` trait

A variable whose type is written as an `of` trait after its name
(`my @arr of Int`, `my @arr[5] of Int`, `my %h{Str} of Int`) used to view as
`my Int @arr`: the parser stored the `of` type as the declaration's type
constraint, and the RakuAST view rendered it before the name.

The parser now marks such a declaration (`VarMarks::type_from_of`, set only
when nothing but the name came before the `of`), and the view renders the type
as the first entry of the declaration's `traits`, a `RakuAST::Trait::Of`, as
rakudo's RakuAST frontend does. Lowering reads the `Trait::Of` back as the type
constraint, so the declaration still runs as a typed container. Found through
the `.DEPARSE` corpus (`S02-types/array-shapes.t`, #14880).
