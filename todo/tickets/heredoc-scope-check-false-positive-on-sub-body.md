# `check_heredoc_scope_errors` false-positives on any `my $x` + later heredoc interpolation inside a sub

## Discovered while

Surveying CSV libraries on the ecosystem (`docs/batteries/csv.md`) for the
Python-stdlib-comparison `csv` gap. `CSV::Table` (`zef:tbrowder`) depends on
`Text::Utils`, whose `Text::Utils::Subs.rakumod` fails to even load under
mutsu.

## Repro

```
$ mutsu -e 'sub foo() { my $x = "hi"; print qq:to/HERE/; value: $x
HERE
}
foo();'
Variable '$x' is not declared. Perhaps you forgot a 'sub' if this was intended to be part of a signature?
  in block <unit> at -e line 1

$ raku -e 'sub foo() { my $x = "hi"; print qq:to/HERE/; value: $x
HERE
}
foo();'
value: hi
```

Minimal, reduced from `Text::Utils::Subs::test-and-show-string-list`
(`~/.zef/store/Text-Utils-4.0.2/*/lib/Text/Utils/Subs.rakumod:157`, which
declares `my $opt-used = ...;` then interpolates `{$opt-used}` inside a
`qq:to/HERE/` a few lines later). Hyphenated names, ternaries, and
indentation are all red herrings — bisected away; the trigger is just "a
`my` local declared earlier in a sub's own statement list, referenced from a
heredoc later in the same sub". A top-level (non-sub) heredoc referencing a
top-level `my` works fine.

## Root cause

`src/compiler/helpers_sub_body.rs:196`, inside
`compile_sub_body_with_deprecation`:

```rust
if let Some(err) = self.check_heredoc_scope_errors(body) {
```

`check_heredoc_scope_errors` (`src/compiler/helpers_block_inline.rs:477`)
collects every `Stmt::VarDecl` name found ANYWHERE in `body` into
`block_locals`, then walks `body` again for any `HeredocInterpolation` that
references one of those names, and treats a hit as an error. Its doc
comment says the intent is to catch a variable "declared inside the block
but not visible in the outer scope (where the heredoc terminator physically
appears in Raku)" — i.e. a narrower gotcha, presumably something like a
heredoc used AFTER an `if`/`else` branch that references a variable `my`'d
only inside that branch, which really would be out of scope in real Raku.

The bug is that this same function is invoked with `body` being the SUB'S
OWN top-level statement list (`helpers_sub_body.rs:196`). In that call, the
"outer scope" and "the scope doing the declaring" are literally the same
list of statements — a `my $x` earlier in the sub body IS visible to a
heredoc used later in that same sub body, per ordinary Raku lexical
scoping. There is no leak to detect there; the check conflates "declared
somewhere in this statement list" with "declared in a scope invisible to
the heredoc site" and fires unconditionally whenever both a `my` and a
`HeredocInterpolation` referencing it coexist in a sub body.

The same helper is also called from `compiler/stmt.rs:1910/1916` and
`compiler/helpers_control_flow.rs:247/253` on `then_branch`/`else_branch` —
those call sites may be the ones the check was actually designed for (a
heredoc that is physically inside one `if`/`else` arm referencing a `my`
from the OTHER arm, which genuinely would not be visible). That narrower
case was not re-verified in this investigation — only the sub-body
false-positive was reduced and confirmed.

## Fix direction

Do not eliminate the check outright without checking whether the
`if`/`else` call sites guard a real bug (write a repro there first: a
heredoc in one branch referencing a `my` declared only in the sibling
branch, confirm mutsu currently gets it wrong, confirm raku rejects it
too). Once that's pinned:

- The `helpers_sub_body.rs:196` call is unconditionally wrong and should be
  removed, OR narrowed to only flag a var that is declared inside a nested
  block (`if`/`while`/`for`/bare `{}`) within `body` but referenced by a
  heredoc OUTSIDE that nested block — i.e. actually walk block nesting
  instead of flattening every `VarDecl` in the whole tree into one
  `block_locals` set regardless of nesting depth or statement order.
- A stricter, still-cheap approximation: only add a name to `block_locals`
  from `VarDecl`s that are NOT in the same top-level statement list being
  searched (i.e. only ones found inside a NESTED sub-body slice), rather
  than scanning `body` itself for both roles.

## Verification

- The repro above should print `value: hi` under mutsu.
- Add a `t/` pin: `my $x` followed by a `qq:to/` heredoc interpolating `$x`
  inside a `sub`, and the same pattern inside a `method`.
- Re-run `CSV::Table`'s own suite (fetch via
  `docs/batteries/csv.md`'s survey tarball, or `zef install CSV::Table` to
  a scratch `inst#` repo) under mutsu — it should get substantially
  further once this loads.
- If the `if`/`else` call sites turn out to guard a real leak, keep a
  regression test for that shape too before removing/narrowing them.
