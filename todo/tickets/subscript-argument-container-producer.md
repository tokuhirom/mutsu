# A subscript argument loses its container unless the callee is a named sub

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `2896304c5` plus ADR-0067's nameless-callee argument producer, which closed
the *accessor* argument for every callee shape and left the *subscript*
argument behind for two of them.

```raku
sub g($y is rw) { $y = 9 }
class S { method take($y is rw) { $y = 9 } }
my @a = 1, 2;

g(@a[0]);                    # raku: [9 2]   mutsu: [9 2]   -- works
S.new.take(@a[0]);           # raku: [9 2]   mutsu: dies, "expects a writable container"
my $r = &g; $r(@a[0]);       # raku: [9 2]   mutsu: the same refusal
```

Both refuse loudly; neither is a silent wrong answer.

## Root cause

Two producers, not one. `g(@a[0])` works through
`compile_rw_chain_index_arg` / the index-writeback protocol
(`emit_index_rw_writebacks`), which is wired into the **`CallFunc`** argument
emitter only. ADR-0067's `MarkRwArgRefContextCallee` producer, which now serves
a method or code-value callee, marks an *accessor read*
(`Compiler::is_accessor_shaped_arg` requires an argument-less
`Expr::MethodCall`) and is inert for an `Expr::Index`. So a subscript argument
reaching a nameless callee has no producer at all.

The container it needs already exists: `array_slot_ref` mints an element cell,
and the `:=` spelling (`my $e := @a[0]`) already produces the right one. This is
the argument twin of `todo/tickets/subscript-receiver-raw-invocant-producer.md`,
which records the same missing producer in *receiver* position.

## Why it is not a one-line extension of the accessor producer

The accessor marker is safe to emit unconditionally because its consumer is
narrow: `try_fast_accessor_read`'s `want_ref` branch answers with a container
only for a zero-argument read of a public `is rw` scalar attribute accessor and
ignores the flag otherwise, so a marker on a shape that cannot consume it costs
nothing. A subscript producer has no such narrow consumer — it would have to
promote the element slot, which is a real mutation of the container's storage —
so it needs either its own runtime gate on the same callee question or the
`CallFunc` path's copy-in/copy-out temp protocol extended to `CallMethod` /
`CallOnValue` / `CallOnCodeVar`. Choosing between those wants its own
measurement of how the two protocols interact when both apply to the same call.

## Two smaller rows found in the same survey, recorded here rather than split

- **`$obj.^lookup('m')($obj, $c.v)`** — raku `9`, mutsu refuses. A `Method`
  object invoked as a code value takes its invocant as positional argument 0, so
  the accessor is positional 1 counting the invocant; the code-value gate reads
  `SubData::param_defs` and skips invocant parameters, so it looks for parameter
  1 of a signature that has one. Fixing it means knowing, from the code value
  alone, that it is a `Method` whose invocant is passed explicitly.
- **`method take(:$y is rw)`** — raku refuses the *declaration* at compile time
  (`Cannot use 'is rw' on optional parameter '$y'`); mutsu accepts it and
  refuses at the call site instead. A parser validation gap, unrelated to
  containers; the same is true for the `sub` spelling.

## Repro

```raku
my @a = 1, 2;
class S { method take($y is rw) { $y = 9 } }
S.new.take(@a[0]);
say @a;            # raku: [9 2]   mutsu: dies "expects a writable container"
```
