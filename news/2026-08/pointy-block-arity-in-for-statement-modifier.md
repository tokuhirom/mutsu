# A multi-parameter pointy block as a `for` statement-modifier operand now honors its arity

`EXPR for LIST` where `EXPR` is a closure with an explicit signature uses that
closure as the loop body, so the loop must consume as many elements per
iteration as the closure's arity. mutsu previously always supplied exactly one
element (the topic `$_`), regardless of the closure's real arity:

```raku
say (-> $a, $b { "$a/$b" } for 1, 2, 3, 4);
```

* `raku`: `(1/2 3/4)`
* `mutsu` (before this fix): `Too few positionals passed; expected 2 arguments but got 1`

## Root cause

`parse_single_modifier`'s `for` arm in `src/parser/stmt/modifier.rs` lowered
*any* closure-valued operand (a pointy block, an anonymous `sub`, a
Whatever-curried expression) uniformly to `Expr::CallOn { target: <closure>,
args: [$_] }` -- a call with exactly one argument, the topic. That is
arity-blind by construction: it is correct for a single-param pointy block
(`-> $x { ... }`, arity 1) purely by coincidence, but wrong for anything with a
different arity.

## The fix

The `for` arm now mirrors the lowering already used for a bare
placeholder-parameter block (`{ $^a ~ $^b } for LIST`, fixed earlier in
`news/2026-08/bare-block-as-statement-modifier-operand.md`): make the closure's
body the loop's body and its own signature the loop's signature, so
`Stmt::For`'s existing multi-param handling consumes N elements per iteration
-- exactly like `for LIST -> $a, $b { ... }` already does for the identical
signature written the other way round. Concretely, `Expr::AnonSubParams`'s
`params`/`param_defs` are mapped onto `Stmt::For`'s own `param`/`param_def`
(single named param), `params`/`params_def` (2+ params), or
`explicit_zero_params` (an explicit empty signature, `-> {}` / `sub () { }`),
matching how the hand-written `for` header parser
(`parse_for_params`/`arrow_lambda_inner`) shapes the same signature.

Two closure shapes are deliberately **excluded** from this lowering and keep
the original "call with $_" treatment, because rakudo genuinely invokes them
one element at a time regardless of their apparent arity:

* `Expr::WhateverCurry` (`* + 1 for @a`) -- a `*` placeholder always curries to
  arity 1, however many times it appears in the expression.
* The implicit-`@_` bare-block form (`{ @_ } for 1,2`) -- its only parameter is
  a synthesized slurpy `*@_` (added by `make_anon_sub` when a signature-less
  block reads `@_`), but rakudo still invokes it once per element. This mirrors
  `bare_block_body` in `src/parser/primary/container/meta_ops.rs`, which
  excludes this exact shape from the placeholder-block-to-`Stmt::Block`
  conversion for the same reason. The synthesized `ParamDef`'s `block_param`
  flag is what distinguishes it from a genuine user-written `-> *@_ { }`.
* A single-param pointy block (`-> $x { ... }`) is left on the original
  "call with $_" path too -- it was already correct (arity 1 == one call with
  one argument, whichever mechanism produces it), so leaving it untouched
  minimizes the diff.

## Parameter-shape matrix (measured against `raku`)

| Shape | Elements consumed per iteration | Matches raku after fix? |
|---|---|---|
| `-> $a, $b { }` | 2 | Yes |
| `-> $a, $b, $c { }`, list not a multiple of 3 | 3 per full chunk, dies on the short final chunk with "Too few positionals passed; expected 3 arguments but got N" | Yes (exact message match) |
| `-> $a, $b = 9 { }` (optional/default) | 2, filling the default when the final chunk is short one element | Yes |
| `sub ($a, $b) { }` | 2 (same lowering as a pointy block) | Yes |
| `-> *@a { }` (bare slurpy) | 1 | Yes |
| `<-> $a, $b { }` (rw) | 2, writes back through both params | Yes |
| `-> $x { }` (single param) | 1 (unchanged, original "call with $_" path) | Yes |
| `{ $^a ~ $^b }` (placeholders) | 2 (already fixed earlier) | Yes, not regressed |
| `{ @_ }` (implicit slurpy) | 1 (kept on "call with $_") | Yes |
| `* + 1` (WhateverCode) | 1 (kept on "call with $_") | Yes |
| `&name` (named sub reference) | not invoked at all -- collected as a plain value once per topic element | Yes |
| `-> { }` (explicit zero params) | dies immediately in raku on the first element; mutsu's expression-context `for` compile path never enforced this (pre-existing, independent of this fix -- see `todo/tickets/for-loop-expression-context-ignores-explicit-zero-params.md`) | No (pre-existing gap) |
| `-> $a, *@rest { }` (mandatory + slurpy) | raku consumes 1 element per iteration (`@rest` always empty); mutsu's chunk-size formula counts the slurpy param toward arity and consumes 2 (pre-existing `Stmt::For` compiler gap, not modifier-specific -- see `todo/tickets/for-loop-slurpy-param-arity-ignores-required-count.md`) | No (pre-existing gap) |

The last two rows are genuine, pre-existing `Stmt::For` bugs unrelated to the
statement modifier (both reproduce with the plain non-modifier `for LIST ->
SIG { }` spelling too) and are recorded separately rather than folded into this
fix.

Pinned by `t/pointy-block-arity-for-statement-modifier.t`, which passes
verbatim under both `raku` and `mutsu` (excluding the two known pre-existing
divergences above).
