# A `for` loop's zero-positional-parameter guard now covers every position

`for LIST -> { ... }` -- a loop whose block declares an explicitly empty
signature -- is supposed to die immediately: rakudo hands the block one element
per iteration, and a zero-count signature rejects that first argument before the
body runs once. mutsu enforced that only for a plain *statement* loop, and even
there it reported the wrong count. The guard now fires wherever the loop appears,
reports what rakudo reports, and covers the other spelling of a zero-count
signature.

## What was wrong

```raku
my $i = 0;
say (for 1, 2, 3, 4 -> { $i++ });    # rakudo: dies.  mutsu: printed (0 1 2 3)
```

Three separate gaps, all pre-existing (they predate the statement-modifier arity
work in `news/2026-08/pointy-block-arity-in-for-statement-modifier.md`, which is
where they were first noticed, and the slurpy/chunk-size rework in
`news/2026-08/for-loop-slurpy-param-chunk-size.md`, which listed the last two of
them as known remaining divergences):

1. **The value-collecting compile path dropped the flag.** `compile_do_for_expr`
   (`src/compiler/helpers_do_expr.rs`) is what compiles a `for` loop whose
   results become a value -- `say (for ...)`, `my @r = do for ...`,
   `sink (for ...)`, `eager for ...`, a `for` as the tail value of a `try`/`do`/
   sub body, and the statement-modifier spelling in any of those positions. It
   never received the loop's `explicit_zero_params` at all and hardcoded
   `explicit_zero_params: false` into the emitted `ForLoopSpec`, so the VM guard
   in `src/vm/vm_for_loop_dispatch.rs` could not fire. `compile_lazy_for_expr`
   dropped it (and `params_def`, and `rw_block`) the same way when rebuilding the
   loop node for its `gather` lowering -- the site of a pre-existing
   `// TODO: thread params_def through compile_lazy_for_expr`, now retired.
2. **The message counted the whole source.** The statement form said
   "expected 0 arguments but got 4" for a four-element source. rakudo says
   "got 1" because it fails on the *first invocation*, which receives one chunk,
   not the whole list.
3. **A named slurpy was not recognised as zero-count.** `for 1, 2, 3 -> *%h { }`
   has zero *positional* parameters -- `*%h` binds named arguments only -- so
   rakudo throws exactly the same "expected 0 arguments but got 1". mutsu ran the
   loop with an empty `%h`.

## The fix

`Compiler::for_zero_positional_params` (`src/compiler/mod.rs`, beside
`for_chunk_arity`) answers the one question the VM guard actually needs: does
this loop's *explicit* signature declare zero positional parameters, i.e. is
rakudo's `.count` for it 0? It is true for the parser's `explicit_zero_params`
(`-> { }`, and the statement modifier's `-> { } for LIST` / `sub () { } for
LIST`) and for a signature whose parameters are all *named* slurpies -- reusing
the same `for_param_is_named_slurpy` predicate `for_chunk_arity` already uses to
keep `*%h` out of the chunk size. A block with no signature at all
(`for LIST { }`, `{ } for LIST`) is deliberately not zero-count: it binds the
topic and has `.count` 1.

Both the statement path (`src/compiler/stmt.rs`) and the expression path now
compute the `ForLoopSpec` field through that helper, and the field was renamed
`explicit_zero_params` -> `zero_positional_params` to match what it now means.
The expression path takes `explicit_zero_params` as an argument, which also
sharpens its "labelled `do { }` lowered to a dummy `for Nil`" special case: a
genuine `for Nil -> { }` is an ordinary (immediately failing) loop, not a `do`
block, so that branch now requires the signature to be absent.

The VM guard reports the size of the first chunk (`spec.arity`, clamped to the
source length) instead of the source length. For a zero-count signature the
chunk is one element, so the message reads "got 1" exactly as rakudo's does.

Threading `params_def`/`rw_block` into the `lazy for` lowering was needed anyway
for the guard, and fixes the arity handling that TODO described as a side
effect: `(lazy for 1, 2, 3 -> $a, $b = 7 { $a + $b }).eager` now yields
`(3 10)`. Laziness is preserved -- the guard runs when the gather is reified, so
`say (lazy for 1, 2, 3 -> { 1 })` still prints `(...)` and only `.eager`/`[0]`
throws, matching rakudo.

## Verification

`t/for-loop-zero-arity-block.t` pins 34 assertions across statement, expression,
`do for`, method-consumed, statement-modifier, `sub () { }`-modifier and
`lazy for` positions; both the throwing and the living (empty-source) shapes;
the exact message text; that the body ran zero times in every failing case; and
that neither a bare topic block nor `-> $a` nor `-> $a, *%h` regressed. The file
passes verbatim under `raku` as well as mutsu.

## Known remaining divergences (out of scope)

* `for 1, 2, 3 -> () { }` is not a zero-parameter signature in rakudo at all:
  the parentheses make it a *destructuring* pattern, and rakudo dies with
  "Cannot unpack or Capture `1`". mutsu runs the loop. That is a signature-binder
  gap, not an arity one.
* `map -> { $_ }, 1, 2, 3` has the same zero-count problem through a different
  mechanism (block invocation by `map`, not the `ForLoop` opcode) and still runs.
* Calling a zero-arity closure directly (`(-> { 42 })(5)`) says "but got more"
  rather than naming the count.
