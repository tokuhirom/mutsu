# Loop labels containing a hyphen (`MY-LABEL:`) are silently ignored by `next`/`last`/`redo`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Label.rakudoc:87,101,120`).

## Root cause

`src/parser/helpers.rs::is_loop_label_name` (around line 558) is used to decide whether an
identifier following `next`/`last`/`redo` (in `src/parser/stmt/simple/control_stmts.rs`) is a
loop label argument or not:

```rust
pub(super) fn is_loop_label_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_uppercase() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
}
```

This rejects any label containing a hyphen. But Raku's kebab-case identifier syntax allows
hyphens in ordinary identifiers, and that includes loop labels — `raku-doc/doc/Type/Label.rakudoc`
uses exactly this style (`MY-LABEL:`) in its own canonical example. Because `is_loop_label_name`
returns `false` for `"MY-LABEL"`, `next_stmt`/`last_stmt`/`redo_stmt` in
`src/parser/stmt/simple/control_stmts.rs` fall through to the unconditional, unlabeled form
(`Stmt::Next(None)` / `Stmt::Last(None)` / `Stmt::Redo(None)`), and the label token itself (plus
any trailing `if`/`unless` statement modifier) is left to be parsed as an unrelated *following*
statement — a bare identifier expression statement, optionally with an `if`/`unless` modifier.

This means:
- `next MY-LABEL if COND;` compiles to `next; MY-LABEL if COND;` (an unconditional `next`, plus a
  dead/harmless expression statement) — the loop body's `next` fires on every iteration regardless
  of `COND`, so nothing after the `next` in the loop body ever runs.
- `last MY-LABEL if COND;` likewise becomes an unconditional `last` — the loop exits on its first
  iteration instead of exiting once `COND` becomes true.
- `redo MY-LABEL unless COND;` becomes an unconditional `redo` — the loop body redoes forever
  regardless of `COND`, hanging (this is what produced the `exit 124` timeout for finding [2] in
  the harness report).

Confirmed with `--dump-ast`: `next MY-LABEL if $_ < 5;` compiles to two separate statements,
`Next(None)` followed by an `If` whose `then_branch` is `Expr(BareWord("MY-LABEL"))`.

Loop *label declarations* (`LABEL: for ... { }`, handled by
`src/parser/stmt/control/labeled_loop.rs::labeled_loop_stmt`) do **not** have this restriction —
they accept any identifier via the ordinary `ident()` parser, hyphens included. So `MY-LABEL:` at
the loop-declaration site parses fine; only the `next`/`last`/`redo` *reference* site (and the
expression-context label check at `src/parser/primary/ident/identifier_call.rs:1139`, which reuses
the same `is_loop_label_name` predicate) reject the hyphenated form.

## Minimal repro

```raku
MY-LABEL:
for 1..10 {
    next MY-LABEL if $_ < 5;
    print "$_ ";
}
```

- `raku`: `5 6 7 8 9 10 `
- `mutsu` (`target/debug/mutsu`): prints nothing (the `next` fires unconditionally every
  iteration).

```raku
MY-LABEL:
for 1..10 {
    last MY-LABEL if $_ > 5;
    print "$_ ";
}
```
- `raku`: `1 2 3 4 5 `
- `mutsu`: prints nothing (the `last` fires unconditionally on the first iteration).

```raku
my $has-repeated = False;
MY-LABEL:
for 1..10 {
    print "$_ ";
    if $_ == 5 {
        LEAVE $has-repeated = True;
        redo MY-LABEL unless $has-repeated;
    }
}
```
- `raku`: `1 2 3 4 5 5 6 7 8 9 10 `
- `mutsu`: hangs (unconditional `redo` loops forever on `$_ == 5`).

## Affected files

- `src/parser/helpers.rs` — `is_loop_label_name` (the character-class predicate itself)
- `src/parser/stmt/simple/control_stmts.rs` — `last_stmt`/`next_stmt`/`redo_stmt`, which gate the
  label-vs-bare-form branch on this predicate
- `src/parser/primary/ident/identifier_call.rs:1139` — the expression-context labeled-loop check,
  which reuses the same predicate

## Suggested next step

Widen `is_loop_label_name` to accept the same hyphen-continuation rule ordinary Raku identifiers
use (mirroring whatever character class `ident()` itself accepts for continuation). Verified with
`raku` that labels are **not** restricted to all-caps either — a lowercase label
(`my-label: for 1..3 { next my-label if $_ < 2; print "$_ "; }`) is legal and works
(`raku` prints `2 3`). So the real fix is likely broader than "allow hyphens": a loop label is just
an ordinary Raku identifier, and the ALL-CAPS-only restriction in `is_loop_label_name` exists only
to disambiguate `next`/`last`/`redo` (which take an *optional* label with no other trailing term)
from other constructs, not because labels are semantically restricted to uppercase. Whatever
replaces this predicate must still avoid misparsing `next SOMETHING-THAT-IS-NOT-A-LABEL` (loop
control statements don't take arbitrary expression arguments, so the disambiguation only needs to
avoid swallowing a following statement/expression that happens to start with an identifier — e.g.
a same-line `next if COND` where `if` is a keyword, not a label).
