# A bare `{ ... }` block is now the statement a statement modifier modifies

`say ({ $_ + 1 } for 1, 2, 3)` printed `(  )` — three uncalled closures — where
raku prints `(2 3 4)`. The bare block was collected as a `Sub` value once per
element instead of being run as the loop body. Reported from the doc-diff
harness (`Language/py-nutshell.rakudoc:541`).

## Root cause

The bug was **not** in how `{ ... }` is disambiguated from a hash literal, and
not in the `for` modifier itself: the *statement*-position spelling
(`{ $n++ } for 1 .. 3;`) already worked. It was the two parsing paths
disagreeing about what the modifier's operand is.

`src/parser/stmt/mod.rs` parses a leading `{` as a **statement** — a
`Stmt::Block` — and hands that to `parse_statement_modifier`. But parenthesised
content is parsed as an **expression**, so `src/parser/primary/container/paren.rs`
reaches `try_inline_modifier` with a closure *term* (`Expr::AnonSub { is_block:
true, .. }`) and wrapped it as `Stmt::Expr(<closure>)`. The `for` modifier's
`Stmt::Expr` arm knows how to invoke a `Lambda` / `AnonSubParams` / Whatever-code
operand (which is why `-> $i { ... } for ...` and `* + 1 for ...` already worked)
but had no case for a plain bare block, so the closure value was simply
collected.

Probing rakudo pinned down the exact semantics to match:

* `for`, `if`, `unless`, `given`, `when`, `with`, `without`: a bare block operand
  is run — `({ 42 } if 1)` is `42`, and `({ $_ + 1 } for 1,2,3)` is a `List`
  `(2 3 4)`.
* `while` / `until` are the exception: rakudo thunks the statement they modify,
  so a bare block genuinely stays an uncalled term there
  (`raku -e 'my $i = 0; say ({ $i } while $i++ < 3)'` prints three `Block`s).
* A pointy block is *not* a bare block — `(-> $x { $x } if 1)` stays an uncalled
  closure in rakudo too.
* `{ a => 1 }` and `{ }` are hash literals, not blocks, and are unaffected.

## Fix

`try_inline_modifier` now converts a bare-block operand into the `Stmt::Block`
that statement position would have produced, so both spellings share one code
path (`while`/`until` excluded, per above). A bare block that declares
placeholder parameters (`{ $^a + $^b }`) is built as an `AnonSubParams` by
`make_anon_sub`; it is told apart from a pointy block by its parameter list being
exactly the placeholders its body declares, which also correctly excludes the
implicit-`@_` form `{ @_ }` (rakudo keeps that one a term).

Two follow-on defects surfaced and were fixed with it:

* `{ ... } for LIST` is the same loop as `for LIST { ... }`, so the block must
  give the loop its implicit placeholder signature. It did not, and
  `{ @r.push($^a ~ $^b) } for (1,2),(3,4)` produced `[True/True True/True]`.
  The signature derivation was extracted from `for_loops.rs` as
  `placeholder_loop_params` and is now shared by both spellings.
* The value-collecting compile path (`compile_do_for_expr`, used when the loop is
  an expression) never marked the modifier's sole body block as the construct's
  own body block, so ADR-0048 D3/D6 treated it as a separately-invoked nested
  block and `({ $^a * 2 } for 1,2,3)` died with "Too few positionals passed".
  It now mirrors the statement path's `note_construct_body_block_stmts` call.

Pinned by `t/bare-block-for-statement-modifier.t`, which passes verbatim under
both `raku` and `mutsu`.

## Known remaining divergences (out of scope, recorded separately)

* `{ ... } while COND` / `until` in *statement* position runs the block in mutsu
  but not in rakudo. Pre-existing, untouched here.
* `-> $a, $b { ... } for 1,2,3,4` should consume two elements per iteration
  (rakudo: `(1/2 3/4)`); mutsu still calls the closure with a single `$_`. See
  `todo/tickets/pointy-block-arity-in-for-statement-modifier.md`.
