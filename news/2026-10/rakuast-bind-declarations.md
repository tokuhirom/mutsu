# RakuAST: binding declarations (`my $x := …`) cross the boundary

`.AST` refused every binding declaration. A bound declaration is
`my $x := $y`, `my @a := @b` or `my %h := %src`. The parser wraps it in a
`SyntheticBlock` of bookkeeping statements, such as the `MarkBind` and
`MarkReadonly` markers and the bound-array length and shape records, and the
converter saw only that block.

Measured on rakudo 2026.09, a binding declaration is the same
`VarDeclaration::Simple` as an assignment, except that its initializer is
`Initializer::Bind`. The bookkeeping now lives in one function,
`ast::bind_decl::expand`, which both the parser and the RakuAST lowerer call.
The converter recognises the expansion with `ast::bind_decl::declaration`,
renders the declaration inside it with an `Initializer::Bind`, and lowering
rebuilds the same expansion.

Running the round-trip ratchet also exposed a test that had passed by chance.
`t/routines/undeclared-routine-compile-time.t` checked the reported line with
a bare `/3/`, which also matched digits in `is_run`'s temporary file name. In
fact the round trip reports every line as 1 (#11125). The test now matches
`line 3` / `:3` exactly, and the file leaves the ratchet until #11125 is fixed.

The round-trip ratchet grows from 1789 to 1910 of 5896 `t/` files. Pinned by
`t/rakuast/rakuast-bind-declaration.t`, which passes under both mutsu and
raku.
