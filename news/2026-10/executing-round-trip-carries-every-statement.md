# The executing round trip carries every statement kind; an expression it cannot carry is converted where it stands

ADR-15334 slice 9. The purity test that decides which statements cross the executing round trip
as a `ParsedLeaf` no longer lists statement kinds: routines and methods of every shape
(defaults, `where` clauses, literal and destructuring parameters, traits, return types, `is
export`), packages, `has`, `token`, constants, enums, assignments and declarations are carried
as the parser built them. An expression a leaf cannot carry (a lowercase bareword to classify, a
regex with code, a placeholder) is converted and lowered on its own while the leaf is lowered,
inside the bareword scopes of the body that holds it, so a statement shell is never converted
for the sake of one operand.

What the node route did around those statements is now done where the leaf is lowered: a
compound assignment lowers from its typed node (a `*` target is the bare `*`, a conditional
target is restored), `$.x = v` is the assignment expression, the unit's `&name` declarations,
sigilless terms and parameters, constants, enums and packages are found by the one scope scan,
a `$` keeps the `state` declaration the parser put with it, and a package's written traits stay
its own fields. `ast_visit::walk_parsed` now hands every parsed body to `visit_stmts_mut`
(ADR-10499), which a pass that tracks scope relies on.

Frontend Ir against the frozen baseline (profiling build, callgrind): zef-cli 117.3% -> 108.3%,
s12-class-attributes 115.1% -> 106.0%, grammar-cursor-attributes 120.3% -> 112.9%; the other two
cases are unchanged (104.7%, 103.1%). #15657 stays open for the grammar case.
