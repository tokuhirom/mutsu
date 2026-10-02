# RakuAST: `my class` and `my grammar` cross the boundary

`.AST` refused every lexical package declaration -- `my class C { … }`, the
common way to declare a helper class inside a test block -- as a "class with
inheritance / scope / repr / traits". Lexical classes caused about 85% of
that refusal under `MUTSU_RAKUAST=1`.

Measured on rakudo 2026.09, a lexical class or grammar is the same node led by
`scope => "my"`, and the default `our` scope renders no field. Both directions
now carry the scope.

Lowering one also showed that a lowered class declaration used `decl_id: 0`,
the "no stable site" sentinel. A lexical class is registered under its name
mangled with its declaration-site id (ADR-0047), so without an id a
round-tripped `{ my class C { } }` leaked `C` into the global namespace. Lowering
is the parser's counterpart, so it now mints the site id the parser would.

The round-trip ratchet grows from 1599 to 1626 of 5822 `t/` files. Pinned
by `t/rakuast/rakuast-lexical-package.t`, which passes under both mutsu and
raku.
