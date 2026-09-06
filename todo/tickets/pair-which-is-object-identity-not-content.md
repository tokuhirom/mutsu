# `Pair.WHICH` is a per-object id, not the pair's content

Found during the 2026-09-06 neighbourhood sweep for the user-defined-`WHICH`
ticket (`news/2026-09/user-defined-which-decides-object-identity.md`). It is
independent of that fix: it reproduces with no user class involved.

## Repro

```raku
say (a => 1).WHICH eq (a => 1).WHICH;
# raku: True    mutsu: False

say (a => 1).WHICH;
# raku:  Pair|767B9AEF58D3B666CA5F43B500F8B0CADC2391FA
# mutsu: Pair|2      <- a per-object counter
```

A `Pair`'s identity is value-based in Raku: it is composed from the key's and
the value's own `.WHICH`. mutsu's `.WHICH` *method* hands back `Pair|{id}` from
the per-object instance counter, so two structurally identical pairs get
different identities and the string is not stable across runs.

## Where to look

The `"WHICH"` arm of `src/builtins/methods_0arg/dispatch_core_coerce.rs` has
per-type cases for `Int`/`Str`/`Set`/`Bag`/`Mix`/... but none for `Pair` or
`ValuePair`, so a Pair falls through to the id-based tail.

The correct string already exists elsewhere: `runtime::utils::value_which_key`
has proper `Pair` / `ValuePair` arms (`Pair|Str|x|Int|1`, recursing through the
key's and value's own keys), and they are what Set/Bag/Mix element keying
already uses. So the fix is very likely to route the `WHICH` method's Pair case
through `value_which_key` rather than to invent a second encoding.

## Note on scope

`===` on pairs is **already correct** (`(a=>1) === (a=>1)` is `True`), because
`runtime::utils::values_identical` compares Pairs structurally without going
through the `.WHICH` method. So the visible damage is limited to the `.WHICH`
string itself and to anything that compares those strings directly. Check
whether anything in roast asserts on the `Pair|...` spelling before changing the
format, and re-check `===`/`eqv`/hash-key behaviour after.
