# `use lib EXPR` with a non-literal path defers the imported module's declarations from the PARSER, breaking later bareword-listop-call parsing

Found while investigating `todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`'s
`01-basic.rakutest` failure ("No matching candidates for proto sub: matches" at test 12). The
CBOR::Simple angle turned out to be a red herring — this is a general, module-system-independent
parser bug, reduced to a 6-line repro with no CBOR involved at all.

## Repro

```
mkdir -p /tmp/mtest/t/lib
cat > /tmp/mtest/t/lib/Foo.rakumod <<'EOF'
unit module Foo;
multi matches(Mu $value, Str:D $cbor) is export { say "str: $value $cbor" }
multi matches(Mu $value, Buf:D $cbor) is export { say "buf: $value" }
EOF
cat > /tmp/mtest/t/dynamic-lib.rakutest <<'EOF'
use lib $*PROGRAM.sibling('lib');
use Foo;
matches -18446744073709551616, '3bffffffffffffffff';
EOF
mutsu /tmp/mtest/t/dynamic-lib.rakutest
```

mutsu:

```
Useless use of constant string "3bffffffffffffffff" in sink context
    at /tmp/mtest/t/dynamic-lib.rakutest:3
No matching candidates for proto sub: matches
  in block <unit> at /tmp/mtest/t/dynamic-lib.rakutest line 3
```

`raku`: prints `str: -18446744073709551616 3bffffffffffffffff` (the correct 2-arg dispatch), no
warning.

## Root cause hypothesis (not yet confirmed by reading the compiler internals)

The determining factor is NOT whether `Foo` is actually found (tested: even when `use lib
$*PROGRAM.sibling('lib')` resolves to a NONEXISTENT directory and `use Foo` fails outright with
"Could not find Foo", the SAME "Useless use of constant string ... in sink context" parse-time
warning still fires for the exact same later statement) and NOT the mere presence of a `use lib
EXPR;` statement (tested: `use lib $*PROGRAM.sibling('lib'); use Foo;` PLUS an explicit `-I
/path/to/lib` on the command line works correctly — no warning, correct dispatch). The determining
factor, isolated by a 2x2 matrix of (module found via `-I` vs found via `use lib EXPR`) x (`EXPR`
literal vs computed):

| how `Foo` resolves | later `matches -NUM, STR` parses correctly? |
| --- | --- |
| `-I /path/to/lib` (command line) | yes |
| `use lib 'lib';` (string literal, relative) | yes |
| `use lib $*PROGRAM.sibling('lib');` (computed, no `-I`) | **no** |
| `use lib $libdir;` where `my $libdir = $*PROGRAM.sibling('lib').Str;` (computed, no `-I`) | **no** |
| `use lib $*PROGRAM.sibling('lib'); use Foo;` PLUS an ALSO-present `-I` for the same dir | yes |

So: a `use lib` argument that is a **literal string** (however it resolves, even relatively) behaves
like `-I` — the imported module's declarations become visible to the PARSER before it reaches later
statements in the same file. A `use lib` argument that is **not a literal** (a method call, or a
`my`-bound variable holding one) requires evaluating an expression to know the path, which can only
happen at RUNTIME — so the import of `Foo` (and therefore its `multi matches` declarations)
necessarily happens interleaved with mainline EXECUTION, not before mainline PARSING. By the time
the parser reaches the `matches -18446744073709551616, '...'` statement (parsing the WHOLE file
before running anything, in mutsu's architecture), it does not yet know `matches` is a declared
multi sub that takes bare listop-style arguments — so it presumably falls back to treating `matches`
as an unrecognized bareword, and the ` -NUM, STR` tail misparses (the unary minus on the first
argument is the specific trigger — every failing `matches` call in `01-basic.rakutest` has a
negative first argument; the positive-number sibling calls parse fine even with the SAME dynamic
`use lib`). This produces two visible symptoms from ONE root cause: the "Useless use of constant
string ... in sink context" warning (the string argument got split into its own statement) and,
when it happens to a multi sub whose only candidates are 2-arity, "No matching candidates" (the
call that actually executes now has only 1 positional arg).

## Why this is `todo/deep`, not a quick ticket

Confirming this requires reading how mutsu's parser distinguishes "known sub, apply listop-argument
parsing" from "unknown bareword" — and whether/how a `RegisterDecl`-style declaration from an
EVAL'd/dynamically-`use`d module can retroactively make itself visible to a parse pass that already
ran ahead of it. This is the same fundamental two-phase (parse-then-run vs. Raku's real
interleaved-compile-and-run) tension CLAUDE.md's "Raku's context-dependent parsing (slangs)" section
already flags as an open architecture question — not a narrow, single-call-site fix. A real fix
likely needs either (a) a genuine BEGIN-time-evaluable `use lib` fast path that runs the path
expression during an early parse pass when possible, or (b) making the parser's bareword-vs-listop
decision robust to "unknown sub name" in a way that still parses `-NUM, STR` as two positional
arguments rather than misparsing — a general parsing-robustness improvement, not a `use lib`-specific
patch.

## Not a `Digest`/CBOR blocker

Confirmed independent of CBOR::Simple/Digest — the minimal repro above uses a throwaway `Foo`
module. `todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`'s remaining
`01-basic.rakutest`/`03`/`04`/`06` triage should be re-attempted with an explicit `-I` (bypassing
the vendored suite's own `use lib $*PROGRAM.sibling('lib');`) to separate this bug's noise from any
other real per-file issues, since the vendored `battery-testsuite.sh` harness presumably invokes
files in place (hitting this bug) rather than via `-I`.
