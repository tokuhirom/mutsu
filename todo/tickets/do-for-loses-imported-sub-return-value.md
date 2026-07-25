# `do for` loses the return value of an imported module sub

Found 2026-07-25 while pinning the sigilless-parameter/native-type-name fix
(`news/2026-07/sigilless-param-shadows-native-type-name.md`). Independent of it —
this reproduces with an ordinary sigiled signature.

**Root cause is known (below); the fix is not written.** Silent wrong data, not
an error.

## Repro

`tmp/mlib/MyPlain.rakumod`:

```raku
unit module MyPlain;
sub plainsub (Str $s) is export { "P:$s" }
```

```raku
use MyPlain;
say do for ^2 { plainsub('x') };        # raku: (P:x P:x)   mutsu: ((Any) (Any))
say do for ^2 -> $i { plainsub('x') };  # raku: (P:x P:x)   mutsu: ((Any) (Any))
say do given 1 { plainsub('x') };       # both: (P:x)
say plainsub('x');                      # both: P:x
```

A plain `for` + `.push`, `.map`, and a **locally declared** sub of the same shape
under `do for` all collect correctly. Three source-level workarounds are
equivalent — wrap the call in parens, assign to a temp first, or interpolate it;
a trailing semicolon does **not** help.

## Root cause

Printing the *executed* op range at the top of the loop epilogue
(`run_start..loop_end` and the opcodes in it) over one file holding the working
parenthesized form and the failing bare forms:

```
parens (OK):   range=6..9    ops=["LoadConst(4)", "ContainerizePair", "CallFunc{…}"]
bare (LOST):   range=21..26  ops=["LoadConst(4)", "ContainerizePair", "CallFunc{…}", "SinkPop(false)", "LoadConst(9)"]
```

The bare form's loop body runs a **`SinkPop`** after the call, which discards the
value. `compile_stmt`'s `Stmt::Expr` arm emits `SinkPop`
(`compiler/stmt.rs:489`); `compile_stmts_value`'s `is_last` arm — the
value-collecting path a `do for` body should use
(`compiler/helpers_control_flow.rs:33`) — does not. So the body's last statement
is compiled through the **sink-statement** path instead of the value path.

This is consistent with the finer measurement below: `CallFunc` pushes the
correct value and the collector faithfully collects the stack top, so the value
is dropped in between — by an opcode.

It also means the framing "imported vs local" is not the real axis: it is "this
statement got recompiled through the sink path". Parens / a temp / interpolation
each change the last statement's shape enough to avoid it.

## Where the fix goes

The runtime recompile path (`compile_block_raw`, or whatever recompiles the
mainline after a module load) and how it chooses `compile_stmt` versus
`compile_stmts_value` for a value-collecting `do for` body — the `collect: true`
context has to reach it.

## ⚠ Measurement trap — read before instrumenting

**`--dump-bytecode` does not show what executes for a program that `use`s a
module.** On the file above it shows op 24 as `SetLocalDecl`, while the
**executed** op 24 is `SinkPop(false)`: the mainline is recompiled at runtime
after the module loads, and only the recompiled chunk carries the `SinkPop`.

Reducing the case to `-e` without the `use` makes it worse, not better — no
recompile happens there, so both forms genuinely compile identically and the
difference vanishes. That combination produced a confidently wrong conclusion
(see below). **Dump the executed op range instead**, from
`exec_for_loop_body`'s epilogue.

## Supporting measurement (still valid)

Two temporary `eprintln`s — one after `OpCode::CallFunc` in
`vm_exec_dispatch.rs`, one at the collect point in `vm_for_loop_body.rs` —
printing the stack top:

```
[DBG-callfunc] plainsub -> top=Some("P:x")   [DBG-collect] stack_len=1 base=0 top=Some("P:x")   # parens: OK
[DBG-callfunc] plainsub -> top=Some("P:x")   [DBG-collect] stack_len=1 base=0 top=Some("")      # bare:   LOST
[DBG-callfunc] plainsub -> top=Some("P:x")   [DBG-collect] stack_len=1 base=0 top=Some("")      # semi:   LOST
```

`CallFunc` pushes the correct value in every case, and the collector collects
whatever is on top — so neither the call path nor the collect arm is at fault.
Ordering is not a variable either: four alternating bare/parens calls alternate
`Nil, ok, Nil, ok`, and four bare calls all fail.

## ~~Superseded conclusion — do NOT act on this~~

An earlier pass recorded, from a `--dump-bytecode` diff, that the failing and
working forms had **byte-identical bytecode**, and concluded that the divergence
was *runtime state* and that "the next investigator should NOT start in the
compiler". **That conclusion was wrong**, for the two reasons in the trap section
above (dump ≠ executed; the `-e` reduction removed the recompile). It is kept
here only so the mistake is not repeated. The real difference is a compiled one.

## Impact

A `do for` over imported routines yields a list of `Any` with no error. Any
script that builds a list this way from a module's exported subs is affected.
Found while writing `t/sigilless-param-named-like-native-type.t`, whose loop
assertion had to be rewritten as an explicit `for` + `.push` to avoid it.
