# `CSV::Table` comment-file suite still fails: a multi-assign-in-loop divergence that a `note` call masks

## Discovered while

Continuing the CSV battery survey (`docs/batteries/csv.md`) after landing the
`return-rw`/`AT-POS` postcircumfix fix (#6250,
`news/2026-08/return-rw-at-pos-postcircumfix-assign.md`) and the nested
method-accessor index-assign fix (#6251,
`news/2026-08/nested-index-assign-through-method-accessor.md`). With both
fixes, `CSV::Table`'s own suite goes from crashing during `TWEAK` to 7/10
files passing outright; `t/2-commented.t`, `t/5-save.t`, and
`t/7-half-matrix.t` still fail.

## `t/2-commented.t` repro (reduced from the failing file)

```
$ MUTSU=target/debug/mutsu
$ FONT=~/.zef/store/Font-AFM-1.24.10/*/lib
$ TU=~/.zef/store/Text-Utils-4.0.2/*/lib
$ ALGO=~/.zef/store/AlgorithmsIT-0.0.4/*/lib
$ CSVDIR=~/.zef/store/CSV-Table-0.0.2/*/
$ $MUTSU -I $FONT -I $TU -I $ALGO -e '
use CSV::Table;
my $t = CSV::Table.new: :csv("'"$CSVDIR"'t/data/commented.csv");
say $t.cell.elems;   # raku: 1 -- mutsu: 4
'
```

`t/data/commented.csv` is:

```
# a commen,age
      name,age
# Sally is my sister
Sally Jean,# she really is
# another comment
#
```

Expected (raku): 1 data row (`["Sally Jean", ""]`), header `["name",
"age"]` — the 4 comment-only/full-comment lines are stripped and dropped
before being counted as candidate rows. mutsu keeps 4 spurious rows.

## Root cause narrowed to `CSV::Table.rakumod`'s TWEAK read loop, not `strip-comment` itself

`Table.rakumod`'s file-read loop (`lib/CSV/Table.rakumod:156-202`):

```raku
LINE: for $fh.lines -> $line is copy {
    my $comment;
    ($line, $comment) = strip-comment $line, :normalize(False), :mark($cchar),
                                             :save-comment;
    if $line ~~ /\S/ {
        @lines.push: $line;
        my $ns = count-substrs @lines.tail, $!separator;
        ...
    }
    ...
}
```

Every attempt to reduce this to a minimal repro **outside** the real class
context (see below) reproduced identical, *correct* output on both raku and
mutsu — the divergence only appears with the actual `CSV::Table` class, the
actual `Text::Utils::strip-comment` sub, and the actual data file.

Things ruled out (all matched raku exactly in isolation):
- The bare `strip-comment` call on each line of `commented.csv` (both engines
  agree: comment-only lines strip to an empty `$line`).
- The `if $line ~~ /\S/ { @lines.push: ... }` filter loop, replicated
  standalone against the same fixture file (both engines kept exactly 2
  lines: the header + 1 data line).
- `@lines.shift` + `@lines.kv` iteration on a small literal array.
- A same-named-parameter reduction (`sub strip-comment($line is copy, ...) {
  ...; return $line, $comment; }` called as `($line, $comment) =
  strip-comment($line, ...)` inside a `for ... -> $line is copy` loop) —
  matched raku exactly.

## The suspicious clue: an inserted `note` call changes the outcome

Adding an unconditional `note "...[$line]..."` right after the multi-assign
(before the `if $line ~~ /\S/` check) inside the *actual* `TWEAK` loop makes
mutsu's behavior flip to the CORRECT one (`@lines.elems` becomes 1, matching
raku) — but the `note`'s own printed value still shows the *unstripped*
`$line` for iterations 3-6 (`# Sally is my sister`, `Sally
Jean,# she really is`, etc.), i.e. the multi-assign's effect on `$line` is
not visible to that immediately-following `note`, and yet the *later*
`if $line ~~ /\S/` check somehow ends up correct anyway once the `note` is
present. Removing the `note` reproduces the original bug (`@lines.elems`
balloons to 5, `cell.elems` to 4).

This is the classic shape of an env/slot write-back ordering bug (see
`CLAUDE.md`'s "env-writeback campaign" pattern and the `env_dirty` dual
store): a compiled local slot for the loop's `$line is copy` variable is
written by the multi-assign, but a subsequent *compiled* read of `$line`
(the `/\S/` match, the `@lines.push`) may read a stale env-mirrored copy
taken before the multi-assign — unless some other statement (here, a
`note` call, which forces a dynamic/string-interpolation path) forces a
slot-to-env (or env-to-slot) resync first. This would explain why:
- Every literal-list / non-class-context reduction failed to reproduce it
  (probably a different compiled shape without the same stale mirror).
- Enabling the module's own `if $debug` prints (`my $debug = 1;`) instead
  **crashes** with an unrelated-looking error (`count-substrs(Package,
  Str)`) rather than fixing it — that path likely still observes the SAME
  stale-`$line` bug but hits it from a different angle (a comment-only line
  wrongly treated as non-empty, triggering `count-substrs` before
  `$!separator` has been auto-detected from the header, at which point it's
  still the `Any`/`Package` sentinel rather than `'auto'` — worth separately
  confirming `$!separator`'s true default and initial-value type).

## Why this needs a real debugger session, not further guessing

The bug requires the exact combination of: a `submethod TWEAK` (class
context, instance attribute reads `$!comment-char`/`$!separator` interleaved
in the loop), a `for $fh.lines -> $line is copy` (real file iteration, not a
literal list), and the multi-value assign `($line, $comment) =
some-sub($line, ...)` where the callee is imported from another module
(`Text::Utils`). Minimal standalone reductions of each ingredient
individually did not reproduce it; some untried combination (file iteration
+ class attribute reads + imported-sub multi-assign, all together) is
load-bearing. This is exactly the case `CLAUDE.md`'s debugging guidelines
recommend `rust-gdb -batch` breakpoints for: break on the compiled bytecode
site that handles the multi-value destructuring assign
(`compiler/expr_closure.rs` / wherever `($a, $b) = expr` compiles, likely an
`OpCode::AssignExpr`-adjacent or destructure op) inside the loop body, and
compare the slot/env state before vs. after the assign, with and without the
`note` call present, to find exactly which read sees a stale value.

## Other failures in the same suite, not yet investigated

- `t/5-save.t`: `save` fails with a printf-arity mismatch
  ("Your printf-style directives specify 3 arguments, but 4 arguments were
  supplied to format '%-*.*s'") — may be related (a stale-value read feeding
  a wrong arg count) or may be an unrelated `sprintf`/`printf` bug. Not
  investigated.
- `t/7-half-matrix.t`: row/column count mismatches (`expected: '5 5'` /
  `got: '6 5'`) and empty-string mismatches — not investigated; may share
  the same root cause (an extra spurious row from the same comment/line
  handling issue) or may be independent.

## Verification (once fixed)

- `MUTSU_FUDGE=1` is irrelevant here (not a roast test); just re-run
  `CSV::Table`'s own suite per `docs/batteries/csv.md`'s `-I` recipe:
  `prove -e '<mutsu> -I lib -I <Font-AFM>/lib -I <Text-Utils>/lib -I
  <AlgorithmsIT>/lib' t/` from the `CSV-Table-0.0.2` store directory.
  `t/2-commented.t`, `t/5-save.t`, `t/7-half-matrix.t` should all go green
  (10/10 files).
