# `do for` loses the return value of an imported module sub

Found 2026-07-25 while pinning the sigilless-parameter/native-type-name fix
(`news/2026-07/sigilless-param-shadows-native-type-name.md`). Independent of that
fix — it reproduces with an ordinary sigiled signature.

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

Only `do for` is affected. `do given` collects correctly, a plain `for` loop with
an explicit `.push` collects correctly, `.map` collects correctly, and a
**locally declared** sub of the same shape collects correctly under `do for`:

```raku
sub localsub($s) { "L:$s" }
say do for ^2 { localsub('x') };        # both: (L:x L:x)
```

So the failing combination is precisely *`do for`* + *a call to an imported
routine* as the loop body's last statement. The collected value is `Any`, i.e.
the body's value is read from somewhere that the imported-sub call path does not
write.

## Three source-level workarounds, all equivalent

Any of these makes it collect correctly:

```raku
do for ^2 { (plainsub('x')) }              # wrap the call in parens
do for ^2 { my $r = plainsub('x'); $r }    # assign to a temp first
do for ^2 { "{plainsub('x')}" }            # interpolate it
```

Only the *bare* call as the body's last statement loses the value. A trailing
semicolon does not help (`{ plainsub('x'); }` still yields `Any`).

## It is NOT a compilation difference — measured 2026-07-25

`--dump-bytecode` on a file containing the parenthesized form and two bare forms
produces **byte-identical opcode sequences** for all three loop bodies (only the
local slot indices differ):

```
     5: ForLoop(...)         20: ForLoop(...)         35: ForLoop(...)
     6: LoadConst(4)         21: LoadConst(4)         36: LoadConst(4)
     7: ContainerizePair     22: ContainerizePair     37: ContainerizePair
     8: CallFunc{name_idx:5, arity:1, arg_sources_idx:None}   (all three)
```

yet at runtime the first yields `["P:x", "P:x"]` and the other two yield
`[Any, Any]`. The full ASTs are identical too (`--dump-ast`, ignoring `SetLine`).
So whatever diverges is **runtime state**, not the emitted code — which rules out
the "loop-body value capture vs. the compiled-call return path" guess this ticket
originally recorded, and means the next investigator should NOT start in the
compiler.

Ordering is not the variable either: with four calls alternating bare/parens the
result alternates `Nil, ok, Nil, ok`, and with four bare calls all four fail.

## Measured further: the call is fine, the value is replaced afterwards

Two temporary `eprintln`s — one after `OpCode::CallFunc` in `vm_exec_dispatch.rs`,
one at the collect point in `vm_for_loop_body.rs` (`if let Some(ref mut coll) =
collected`) — printing the stack top, over the three-loop file above:

```
[DBG-callfunc] plainsub -> top=Some("P:x")     [DBG-collect] stack_len=1 base=0 top=Some("P:x")   # parens: OK
[DBG-callfunc] plainsub -> top=Some("P:x")     [DBG-collect] stack_len=1 base=0 top=Some("")      # bare:   LOST
[DBG-callfunc] plainsub -> top=Some("P:x")     [DBG-collect] stack_len=1 base=0 top=Some("")      # semi:   LOST
```

So:

- **`CallFunc` pushes the correct value in every case** — the imported-sub call
  path is not at fault, and neither is the collector (it faithfully collects
  whatever is on top).
- Between the call and the collect point, the stack still has exactly **one**
  element (`stack_len=1 base=0` in all six iterations) but in the bare case that
  single element has been **replaced** by `Any`.

Nothing is popped and re-pushed — the count never changes — so whatever runs
between the two points overwrites the slot in place, or the pushed value is an
alias (a `ContainerRef`/topic cell) whose target is reset by the loop epilogue.
`spec.restore_topic` is `true` for this loop, and the epilogue also runs
`write_back_for_topic_item` / `write_back_to_source_var`; those are the first
things to check. Why the parenthesized form escapes it is still unexplained and
is the crux.

## ROOT CAUSE FOUND (2026-07-25): the executed body contains a `SinkPop`

Printing the *executed* op range at the top of the epilogue
(`run_start..loop_end` plus the opcodes in it) gives:

```
parens (OK):   range=6..9    ops=["LoadConst(4)", "ContainerizePair", "CallFunc{…}"]
bare (LOST):   range=21..26  ops=["LoadConst(4)", "ContainerizePair", "CallFunc{…}", "SinkPop(false)", "LoadConst(9)"]
```

The bare form's loop body runs a **`SinkPop`** after the call — which discards
the value — while the parenthesized form does not. `compile_stmt`'s
`Stmt::Expr` arm emits `SinkPop`; `compile_stmts_value`'s `is_last` arm (the
value-collecting path a `do for` body should use) does not. So the body's last
statement is being compiled through the **sink-statement** path instead of the
value path.

### Why the earlier "identical bytecode" measurement was wrong

`--dump-bytecode` on the same file shows op 24 as `SetLocalDecl`, but the
*executed* op 24 is `SinkPop(false)`. **The dumped and executed bytecode differ**
— the mainline is recompiled at runtime (after `use` has loaded the module), and
only the recompiled chunk carries the `SinkPop`. The earlier full-file bytecode
diff also used an `-e` program with no `use`, where the imported name is unknown
and no recompile happens; that is why both forms looked identical. **Do not trust
`--dump-bytecode` for a program that `use`s a module — dump the executed range
instead.**

That also explains every observation: it is not "imported vs local" per se, it is
"this statement was recompiled through the sink path". Parens/temp/interpolation
all change the last statement's shape enough to avoid it.

## Where to look

The runtime recompile path (`compile_block_raw` / whatever recompiles the
mainline after a module load) and how it chooses `compile_stmt` vs
`compile_stmts_value` for a value-collecting `do for` body — the `collect: true`
context must reach it. `compiler/stmt.rs:489` (`Stmt::Expr` → `SinkPop`) versus
`compiler/helpers_control_flow.rs:33` (`compile_stmts_value`).

## Impact

Silent wrong data, not an error — a `do for` over imported routines yields a list
of `Any`. Any script that builds a list this way from a module's exported subs is
affected. Found while writing `t/sigilless-param-named-like-native-type.t`, whose
loop assertion had to be rewritten as an explicit `for` + `.push` to avoid it.
