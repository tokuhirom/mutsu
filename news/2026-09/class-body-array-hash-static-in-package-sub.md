# A class-body `my` array/hash static read empty from a package `sub`

A `sub` (plain or `our sub`) declared inside a class body, whose body read a
`my`-declared `@`/`%` static from the same class body, saw an empty
Array/Hash instead of the initialized value — even though the class body's
`my @arr = (...)` statement had already run before the sub was ever called.
The equivalent case for a scalar static, and for a *method* reading any of
the three, was already correct.

```raku
class Foo {
    my @abbrs = < Muh Saf R.A R.T J.A J.T Raj Sha Ram Shw Qid Hij >;

    our sub month-abbr(Int:D $month) {
      return @abbrs[$month - 1];
    }
}
say Foo::month-abbr(6);   # was: (Any); now: J.T
```

## Root cause

`persist_class_body_statics` mirrors a class body's `my`/`state` top-level
lexicals into `package_lexicals[ClassName]` so they stay reachable once the
defining frame's env is gone. The scalar free-variable read opcode
(`GetGlobal`) already consulted this store (via `package_scope_lexical`)
before falling back to `env`. The `@`/`%` read opcodes (`GetArrayVar` /
`GetHashVar`) never did — they went straight to `env`, which for a
class-body-declared `@`/`%` static holds only the pre-initialization
declaration-time snapshot (an empty Array/Hash), since `persist_class_body_statics`
deliberately removes the bare env binding once the value has been mirrored
into `package_lexicals` (leaving `package_lexicals` as the sole authoritative
store, exactly as it already was for a method).

Fixed by adding the same `package_scope_lexical` consultation, in the same
position (before `env`), to `GetArrayVar` and `GetHashVar`.

Found via `Date::Calendar::Hijri` 0.1.0's `lib/Date/Calendar/Hijri/Names.rakumod`,
whose `unit class` declares a `my @month-abbr = (...)` array static read back
by an `our sub month-abbr`.

Regression test: `t/oo/class/class-body-static-in-named-sub.t`.
