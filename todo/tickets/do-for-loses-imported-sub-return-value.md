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

## Where to look

Since the bytecode is identical, look at what the `CallFunc` execution path does
differently for an imported routine while a `ForLoop` with `collect: true` is
active — e.g. whether the value is left on the stack versus published through the
topic/`_` slot that the collector reads. `vm_for_loop_*` (the collect arm) and the
imported/dynamic-sub return path (`call_function_def` → the env merge) are the two
sides to instrument. Do this by measuring, not by reading: put a temporary
`eprintln` at the collect point and at the call's return and compare the two
forms.

## Impact

Silent wrong data, not an error — a `do for` over imported routines yields a list
of `Any`. Any script that builds a list this way from a module's exported subs is
affected. Found while writing `t/sigilless-param-named-like-native-type.t`, whose
loop assertion had to be rewritten as an explicit `for` + `.push` to avoid it.
