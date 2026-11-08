# `.DEPARSE` parity corpus ratchet

`scripts/ast-text-corpus.sh` now has a `CORPUS=deparse` mode (#15792, ADR-14845 §2.5): the
`.DEPARSE` text of every top-level statement of a pinned sample of roast files, hashed from
rakudo 2026.09 in `ci/deparse-corpus/rakudo.tsv`, is compared with mutsu's. The 2183
statements that match today (90.5% of 2412 in 81 files; the S15-normalization files, ~60 s each under `.DEPARSE`, are left out so the check does not depend on machine speed) are listed in
`ci/deparse-corpus/identical.tsv`, which only grows; CI's `test-suites` job runs
`CORPUS=deparse scripts/ast-text-corpus.sh check` next to the `.AST` text ratchet.
Since the deparser is vendored rakudo code, a mismatch is a difference in the tree it reads
or in a mutsu primitive it uses.

A first fix found by the corpus: `%(:foo, :bar)` keeps its bare colonpairs as `ColonPair::True`
(it was `FatArrow(foo, True)`), so it deparses as `%(:foo, :bar)`.

The corpus also found the model accessors the vendored deparser calls and mutsu lacked
(each measured against rakudo): `Var::NamedCapture.colonpairs`, `VarDeclaration::Anonymous`
`traits`/`twigil`/`shape`/`where`/`original-type`, `Call::QuotedMethod.args`,
`Type::Coercion.constraint` and `Type::Enum.clean-clone`/`traits`. Without them `.DEPARSE`
died on a whole file at its first `$<a>`, `my $ = ...`, `$x."m"()`, `Int()` or `enum`.

`(NaN)i` / `$x\i` are `ApplyPostfix(operand, Postfix "i")` (the parser's `postfix:<i>` call was rendered as a plain call).
