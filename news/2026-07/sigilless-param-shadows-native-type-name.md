# A sigilless parameter named after a native type now shadows the type

A sigilless parameter whose name coincides with a native type read the **type
object** instead of its argument — but only for a routine in a `use`d module:

```raku
# lib/M.rakumod
unit module M;
sub probe (Str \str) is export { "name={str.^name} defined={str.defined}" }
sub other (Str \zzz) is export { "name={zzz.^name}" }
```

```
mutsu was:  name=str defined=False   |  name=Str
raku:       name=Str defined=True    |  name=Str
```

`\int` behaved the same way (binding the `int` type object), while `\zzz` and a
sigiled `$str` were always fine. Declaring the same sub in the main script — or
in an in-file `module M { … }` / `package P { … }` — also worked, which is what
made it look arbitrary.

## Root cause

A bare word that names a sigilless binding must compile to a read of that
binding. The compiler handles two cases: a **same-frame** sigilless local, and a
sigilless binding from an **enclosing** scope (which a separately-compiled module
body sees, since the signature reaches that compilation through
`enclosing_sigilless`). The same-frame branch already got this right, and says so:

> Sigilless bindings and in-scope constants: the bare word IS the variable, so
> read directly from the local slot — even when the name coincides with a native
> lowercase type (`my \str`, an `Int \ch` param): in Raku the lexical binding
> shadows the native type within its scope.

The enclosing-scope branch carried an extra `&& !is_builtin_type(name)` guard —
and `is_builtin_type` includes the lowercase natives `str`, `int`, `num`,
`array`, `byte`, `int8`…`uint64`, `blob8`…`buf64`. So `\str` fell past it to
`GetBareWord`, which consults the type registry and returns the type object.

## Fix

The guard is gone: an enclosing sigilless binding shadows a same-named type,
exactly as a same-frame one does.

`String::Rotate` (`TODO_dist` T-057) now passes **136/136** — its signature is
`sub rotate (Str(Any) \str, Int \ch = 1 --> Str)`, and this was the last of the
dist's three root causes (after the sigilless-`$_` reread and the builtin-shadow
dispatch fixes earlier the same day).

Pin: `t/sigilless-param-named-like-native-type.t` with `t/lib/NativeNameSigilless.rakumod`
(10 tests, identical output under `raku`) — `\str`/`\int`/`\num` parameters, a
non-type-named control, the dist's coercion + defaulted-sigilless shape, the
loop-topic form, the same-frame case, and an unshadowed `str` that must still
name the type.

## Found while pinning

`do for` loses the return value of an imported module sub — `do for ^2 {
plainsub('x') }` collects `(Any) (Any)` where a local sub, a method, `.map` and
`do given` all collect correctly. Unrelated to this fix (it reproduces with an
ordinary sigiled signature) and filed as
`todo/tickets/do-for-loses-imported-sub-return-value.md`; the pin's loop
assertion uses an explicit `for` + `.push` to avoid it.
