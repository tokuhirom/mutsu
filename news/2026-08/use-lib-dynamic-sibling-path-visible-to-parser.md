# `use lib $*PROGRAM.sibling(...)` now registers its module's subs with the parser

Confirmed and fixed the root cause behind
`todo/deep/use-lib-dynamic-path-defers-declaration-visibility-to-parser.md`: `use lib
$*PROGRAM.sibling('lib');` (a non-literal `use lib` argument, as opposed to `use lib 'lib';`) did
not make the imported module's declared subs visible to the PARSER, which corrupted later
listop-style call parsing in the same file whenever the first argument started with a unary minus.

## Root cause

mutsu parses a whole file before running any of it, so `use lib EXPR;` can only expand the module
search path *during parsing* (in time to affect later statements in the same file) when `EXPR` is
one of a small family of expressions the parser can statically evaluate without running the VM —
`src/parser/stmt/simple/lib_paths.rs`'s `extract_lib_path`/`extract_program_parent`, which handled
string literals and `$*PROGRAM.parent(N).add(...)`/`.child(...)` chains. `$*PROGRAM.sibling(...)`
was not one of the recognized shapes, so a `use lib $*PROGRAM.sibling('lib');use Foo;` sequence
left the parser's `LIB_PATHS` untouched, `find_module_file` failed to resolve `Foo` at parse time,
and `Foo`'s `multi matches(...)` declarations never reached `register_user_sub` — even in cases
where `Foo` loaded successfully at runtime moments later.

With `matches` unknown to the parser, `matches -18446744073709551616, '3bffffffffffffffff';` fell
through the bareword-vs-listop-call decision in
`src/parser/primary/ident/identifier_call.rs::identifier_or_call` (the `next == '-'` case is
deliberately excluded from the "known term-start" whitelist there — the file's own comment: *"A
bare `+`/`-` is left out on purpose: `pi - 1` must stay a subtraction, which needs term-vs-listop
knowledge we lack here."*). So `matches - 18446744073709551616` parsed as ordinary infix
subtraction on the bareword `matches`, the trailing `, '3bffffffffffffffff'` turned the whole
statement into a comma-joined `ArrayLiteral` in sink context, and two symptoms fell out of that one
misparse: a "Useless use of constant string ... in sink context" warning for the stranded string,
and "No matching candidates for proto sub: matches" once `matches` actually got called with the
wrong arity.

That `identifier_call.rs` exclusion is intentional and correct — genuinely unknown barewords must
stay ambiguous between "0-arg term minus a number" and "listop call with a negative first
argument," and guessing wrong there would be a worse regression than this bug. The real fix is
direction (a) from the original investigation: widen the *already-existing* family of
parse-time-evaluable `use lib` argument shapes, not loosen the bareword/listop heuristic.

## Fix

Added a `.sibling(NAME)` arm to `extract_lib_path` in `src/parser/stmt/simple/lib_paths.rs`,
alongside the existing literal-string and `.parent(N).add(...)`/`.child(...)` handling. Since the
parser already knows the current file's own path (`PROGRAM_PATH`, set before parsing begins),
`$*PROGRAM.sibling(NAME)` reduces to `dirname($*PROGRAM) + "/" + NAME` — no VM evaluation needed,
so it can be resolved at parse time exactly like the other members of that family. This is the
idiom used by `roast/S12-traits/precomp.t` (`use lib $*PROGRAM.sibling("custom");`) and is the
standard "load a `t/lib` sibling directory" pattern in Raku test suites.

The narrower `my $libdir = $*PROGRAM.sibling('lib').Str; use lib $libdir;` variant (a `use lib`
argument that is a bound variable rather than a direct method-call chain) remains unresolved by
design — it would need cross-statement constant folding during parsing, a materially different and
much larger problem than widening one static-evaluation family, and nothing in the corpus currently
depends on it. If it turns out to matter later it is worth its own ticket rather than folding into
this fix.

## Regression test

`t/use-lib-dynamic-sibling-path.t`, spawning `t/fixtures/use-lib-dynamic-sibling/probe.rakutest`
(a `use lib $*PROGRAM.sibling('lib'); use Matcher; matches -5, 'hello';` script backed by a two-multi
`Matcher.rakumod` fixture) as a subprocess and asserting a clean exit, the correct 2-arg dispatch
output, and no stray warning on stderr. Verified the test fails with the pre-fix parser (all three
assertions) and passes with the fix.
