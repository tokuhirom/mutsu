# RakuAST: signature declarations keep their source form

`my ($a, @b) = 1, 2, 3` is one node in rakudo 2026.09,
`VarDeclaration::Signature(signature => …, initializer => Initializer::Assign(…))`.
mutsu's parser expands it into a staging temporary plus one declaration per
element, and `.AST` saw only that expansion (`SyntheticBlock`) and refused it --
the second most frequent refusal under `MUTSU_RAKUAST=1`.

This is the first construct moved to ADR-10723 Stage 1's pattern for a parser
expansion. The declarator-list parser now builds a `SignatureDecl` record and
hands it to one expansion function (`parser::stmt::decl::destructure::desugar`).
The expansion starts with the record, as a new `Stmt::SourceForm` statement the
compiler emits nothing for. `.AST` renders `VarDeclaration::Signature` from the
record -- parameters as `default-rw` variable targets, `scope` for `our`/`state`,
`Initializer::Assign` or the new `Initializer::Bind` -- and `EVAL` builds a record
from the node and calls the same expansion, in statement and in expression
position (`if my ($a, $b) = f() { … }`, `(my ($a, $b) = 3, 4)`).

Three places recognised the expansion by its shape -- the sink-context warning,
the BEGIN-time group-declaration split and the group declaration as an
expression or X/Z operand -- and now skip the record or share
`ast::is_group_declaration`. Elements with a type, default, constraint or trait,
sigilless and literal elements, named and nested groups are still refused.

The round-trip ratchet grows from 1113 to 1135 of 5733 `t/` files. Pinned by
`t/rakuast/rakuast-signature-decl.t`, which passes under both mutsu
and raku.
