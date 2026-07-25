# A user sub with a default parameter now shadows a same-named builtin

In raku a lexical `sub abs` — declared in the file or imported from a module —
wins over CORE's. mutsu got that right only for some signatures:

```raku
sub rotate (Str $s, Int $n = 1 --> Str) { "R:$s/$n" }
sub abs    (Str $s, Int $n = 1 --> Str) { "A:$s/$n" }

say rotate('x', 3);   # raku: R:x/3   mutsu was: Nil    <- the BUILTIN ran
say abs('x', 3);      # raku: A:x/3   mutsu:     A:x/3  <- user sub ran
say abs('x');         # raku: A:x/1   mutsu was: died inside the builtin abs
```

The pattern was sharp: the user's sub lost **exactly when the call's argument
count matched a native builtin of the same name**. `abs('x', 3)` won because
there is no 2-arg native `abs`; `abs('x')` and `rotate('x', 3)` lost because
1-arg `abs` and 2-arg `rotate` exist natively. Losing was silent — the wrong
routine simply ran.

Found in `String::Rotate` (`TODO_dist` T-057), whose module exports
`sub rotate (Str(Any) \str, Int \ch = 1 --> Str)`.

## Root cause

The VM's named-call path resolves the single candidate and, when the name is a
builtin, applies a deliberately strict gate before running it:

```rust
let gate_ok = if is_builtin {
    // Genuine builtin shadow: strict gate (no default —
    // name-cache pollution hazard, PR #3546).
    Self::def_is_otf_compilable(&def)
} else { … };
```

`def_is_otf_compilable` rejects a signature with a **default parameter**. That
rejection is correct — OTF-compiling such a def pollutes the name-keyed call
caches, which is what #3546 fixed. The bug was what happened next: control fell
through to `call_function_fallback`, which consults the native builtin table
*before* resolving a user routine. So a rejected shadow ran the builtin rather
than the user's def through the interpreter.

`Int $n = 1` is an entirely ordinary signature, so any module exporting a
builtin-named sub with a default was affected.

## Fix

`call_function_fallback` now checks for a user-declared routine before reaching
for the native table, and skips the native table when one resolves. The check is
gated on the name actually being a builtin, so for every other name the lookup
is a single miss and the path is byte-identical to before. The strict OTF gate is
untouched — the shadow now simply runs through the interpreter, which is what the
gate's rejection was asking for.

Pin: `t/builtin-shadow-default-param.t` (9 tests, identical output under `raku`)
— the defaulted shadow at both arities, the no-default shape that already worked,
and the unshadowed builtin sub and method that must keep dispatching natively.
`t/imported-sub-shadows-builtin.t` continues to pass.

## String::Rotate

The dist's two halves had two different root causes. The `method rotate` half was
fixed earlier the same day (`sigilless-param-rereads-reset-topic.md`) and passes,
68/136. The `sub rotate` half is *called* correctly now, but hits a third,
unrelated bug: its parameter is named `\str`, and a sigilless parameter named
after a native type reads the type object inside a module routine. That is filed
as `todo/tickets/sigilless-param-named-like-a-native-type.md`.
