# A `for` loop's pointy signature now handles a trailing slurpy

`for LIST -> $a, *@rest { ... }` was broken twice over: it did not parse as a
`for` loop at all, and the statement-modifier spelling that *did* parse consumed
the wrong number of elements per iteration. Both are fixed, along with the
chunk-representation and rw-writeback fallout that forcing a one-element chunk
exposed.

## What rakudo actually does

Establishing the reference behaviour first mattered here, because the obvious
guess ("chunk by the number of required params") is wrong in both directions.
Rakudo keys a `for`/`map` block's per-iteration chunk size on the block's
`.count`, and batches `count` elements per call *except* when `count` is `Inf`
or below 2, which both mean one element at a time. That single rule explains
every row of the matrix:

| signature | rakudo `.count` | elements per iteration |
| --- | --- | --- |
| `-> $a, *@rest` | `Inf` | 1 (`@rest` always empty) |
| `-> $a, $b, *@rest` | `Inf` | 1 — then dies "Too few positionals passed; expected at least 2 arguments but got only 1" |
| `-> *@all` | `Inf` | 1 |
| `-> $a, +@r` / `-> $a, **@r` | `Inf` | 1 |
| `-> $a, $b = 9` | 2 | 2 (short final chunk triggers the default) |
| `-> $a, $b?` | 2 | 2 |
| `-> $a, *%h` | 1 | 1 (a *named* slurpy is invisible to the positional count) |
| `-> $a, $b, *%h` | 2 | 2 |

So an optional or defaulted **non-slurpy** trailing param still counts toward
the chunk, and only a **positional** slurpy collapses the chunk to one element.

## Root cause 1 — the header parser could not see a slurpy at all

`parse_for_pointy_param` (`src/parser/stmt/control/for_params.rs`) is the
`for`-header copy of `pointy_param::parse_pointy_param`, and it was missing that
parser's slurpy-prefix block entirely. A leading `*` therefore made `var_name`
fail, the `->` parse failed, and the whole statement fell back to being parsed as
a comma-separated expression list — which is why `for 1, 2, 3, 4 -> $a, *@rest
{ ... }` printed four "Useless use of constant integer ... in sink context"
warnings and ran no loop. `*@r`, `**@r`, `+@r` and `*%h` are now accepted (and a
type constraint may precede them, as in `-> Int *@r`).

A lone sigil'd slurpy (`-> *@all`) is additionally routed into the
`params`/`params_def` (plural) shape rather than the singular `param` slot, since
it binds a *list* of the chunk rather than the chunk element; the statement
modifier's `closure_signature_as_for_params` mirrors that routing so both
spellings lower identically.

## Root cause 2 — the chunk size counted the slurpy

`Stmt::For`'s chunk size was `params.len()`, counting a trailing slurpy as if it
were an ordinary positional (and the parallel `required_arity` guard's filter
`default.is_none() && !optional_marker` did the same). `Compiler::for_chunk_arity`
now implements rakudo's `.count` rule directly: a positional slurpy returns 1, a
named slurpy is skipped, and everything else contributes, floored at 1. Both the
statement form (`src/compiler/stmt.rs`) and the expression/collecting form
(`src/compiler/helpers_do_expr.rs`) use it. `required_arity` likewise ignores
slurpies, and the message switches to rakudo's open-ended wording ("expected at
least N arguments but got only M") when a positional slurpy is present.

The binder in `build_for_bind_stmts` learned about slurpies too: a positional
slurpy binds `_.skip(n).Array` (a fresh per-iteration `Array` of the chunk's
unconsumed tail) and a named slurpy binds an empty hash, neither of which
consumes a positional slot. Previously a slurpy fell through to the plain
`_[i]` element bind, which produced a stray `Nil`/out-of-range `Failure`.

## Root cause 3 — an `arity == 1` multi-param chunk was ambiguous

Forcing the chunk to one element exposed a latent representation problem: the VM
only wrapped items into a chunk array when `arity > 1`, so at `arity == 1` the
body received the bare source element. That is indistinguishable from a chunk
when the element is itself a list — `for (1,2),(3,4) -> $a, *@rest` bound `$a`
to `1` and `@rest` to `[2]` instead of binding `$a` to the whole `(1,2)`.
`ForLoopSpec::chunks_items()` now says a multi-parameter signature always gets a
chunk array, whatever the arity (the eager and lazy loops both use it), which is
byte-identical to the old behaviour everywhere except the new slurpy case.

Two consequences of an `arity == 1` multi-param loop had to be handled with it:

* `writes_back_topic` (the implicit-topic `for @a { $_ = ... }` writeback) keyed
  only on "no named param and `arity <= 1`", which a multi-param slurpy loop now
  satisfies. It would have written the chunk back over each source element; it
  now also requires `multi_param_names` to be empty.
* `write_back_for_rw_param`'s multi-param branch was gated on `arity > 1`, so
  `for @a <-> $v, *@r { $v = $v * 10 }` fell into the single-param branch, read
  `$_` (the chunk) and wrote it over the element. The gate is now simply "there
  are rw param names".

## Verification

`t/for-loop-slurpy-param-arity.t` pins 32 assertions covering the whole matrix
above in both plain-statement and statement-modifier position, plus list-valued
elements, a destructuring param beside a slurpy, `<->` writeback, slurpy
freshness per iteration, and `Seq`/`lazy` sources. The file passes verbatim under
`raku` as well as mutsu. `make test` (3491 files) and a 244-file whitelisted roast
sweep over `S04-statements`, `S04-statement-modifiers`,
`S04-blocks-and-statements`, `S06-*`, `S02-lists`, `S03-binding`, `S29-*` and
`S32-{list,array,hash}` are green.

## Known remaining divergences (out of scope, pre-existing)

* A capture parameter (`-> |c`) is still not accepted in a `for` *header*, and
  where it does parse (statement-modifier position) it binds an approximation of
  the leftover chunk rather than a real `Capture`, so `c.raku` reads `[1]` where
  rakudo says `\(1)`.
* `for 1, 2, 3, 4 -> *%h { ... }` has zero positional params, so rakudo throws
  "Too many positionals passed; expected 0 arguments but got 1"; mutsu runs the
  loop with an empty `%h`. mutsu's existing `explicit_zero_params` guard (`-> {}`)
  also reports the whole list length rather than 1 in its message.
