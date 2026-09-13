# `f(++$p)` keeps the native `is rw` reference

`sub outer(int $p is rw) { inner(++$p) }` used to die with

```
Parameter '$p' expects a writable container (variable) as an argument,
but got '6' (Int) as a value without a container.
```

where rakudo binds the reference straight through, so `inner`'s `--$p` reaches
`outer`'s caller and the two operations cancel. Fixed: the argument now
compiles as the two things it means.

## Why rakudo accepts it

A *native*-typed `is rw` parameter is not bound to a `Scalar`; it is bound to a
native reference to the caller's storage. Rakudo's native `prefix:<++>` writes
through that reference and hands the reference back, so the result is still a
writable location and the next callee's own `is rw` parameter binds it.

The shape is narrow, and deliberately so. Rakudo rejects every neighbouring
spelling:

| argument | rakudo |
| --- | --- |
| `++$p`, `$p` a native `is rw` parameter | accepted |
| `++$x`, `$x` a plain `my int` lexical | `Expected a modifiable native int argument for '$p'` |
| `++$p`, `$p` a non-native `$p is rw` | `expects a writable container ... as a value without a container` |
| `$p++` (postfix), any operand | `Expected a modifiable native int argument for '$p'` |

Postfix yields the old value and a `Scalar`'s `prefix:<++>` yields a value, so
neither can be a location; a plain native lexical is not a reference to begin
with.

## The fix

`Compiler::compile_call_arg_with_escape` is the single place every positional
argument passes. It now recognises `++$p` / `--$p` whose operand is one of the
*enclosing routine's* native `is rw` parameters — compile-time information the
compiler already has in the signature it is compiling — and splits it: emit the
increment for its effect, discard its value, then compile the bare variable as
the argument. The bare-variable path already hands a parameter the caller's
container correctly, so nothing new is needed on the binding side, and no
runtime gate is involved (unlike the ADR-0067 accessor markers, the operand's
own declaration settles this at compile time).

Every operand shape rakudo refuses falls through to the pre-existing value
path, so it keeps failing exactly as it did. The new
`native_rw_params` set on `Compiler` is seeded per routine body and inherited
by nested blocks, since the parameter stays lexically visible there.

Pinned by `t/routines/signature/rw-param-incdec-arg-container.t`, whose eleven
assertions cover the four rakudo behaviours in the table above, the relay
across three frames, and the fact that the increment still happens when the
callee does not want a container at all.

## What it unblocks

`JSON::Fast`'s whitespace scanner hands a pre-incremented position straight to
its comment scanner (`nom-comment($text, ++$pos)`), which un-eats the character
with `--$pos` when JSONC comments are not allowed. Without the reference
reaching back, `from-json` concluded it had consumed the whole string and
accepted trailing garbage — `from-json(q<{"a":"b"}/>)` returned `{:a("b")}`
instead of complaining. One of the two blockers ([#8233]) on running the real
upstream distribution ([#8226], ADR-0096 §D4/E2).

[#8226]: https://github.com/tokuhirom/mutsu/issues/8226
[#8233]: https://github.com/tokuhirom/mutsu/issues/8233
