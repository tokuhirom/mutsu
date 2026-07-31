# Expression-position `my` no longer trips `use strict`

`use strict; ok my $x = 7, 'desc'` died with
`X::Undeclared: Variable '$x' is not declared` — the Humming-Bird
`t/03-response_decoding.rakutest` blocker from the web-framework survey
(the file `use`s strict directly, and Humming-Bird::Core's own `use strict`
leaks interpreter-wide, so every HB test ran strict).

Root cause: an expression-position declaration (`ok my $x = ...`,
`(my @a = 1,2)`) compiles to a plain `SetGlobal` store with no local slot,
and the strict checker in the VM could not tell that declaring write from an
undeclared assignment. Statement-position declarations already emit
`MarkVarDeclContext` before their store; the expression path now does the
same (scalar, native-int, typed/untyped-Nil, `our`, and bare-container
branches), and the strict check exempts a store arriving with
`vardecl_context` set. A non-declaring `$x = 5` under strict still errors.

Pin: `t/strict-expr-position-decl.t` (7/7 under raku too). Effect:
Humming-Bird's upstream suite reaches its raku baseline of 10/14
(9/14 → 10/14 on top of #5599; the four remaining files fail under raku
itself with a duplicate-import error, and t/04 / t/13 have separate,
deeper mutsu gaps).
