# `Any`/`Nil` has no `.Int`/`.Num` coercion method (only `.Str` works)

`my $value = Any; $value.Int` (and `.Num`) raises `No such method 'Int' for
invocant of type 'Any'` instead of Rakudo's "Use of uninitialized value ... in
numeric context" warning + `0`. `.Str` on the same value already produces the
correct warning-and-empty-string behavior, so this is specifically a gap in the
`.Int`/`.Num`/`.Numeric`/`.Real` coercion methods for the *value* case (a bare
`Any`/`Nil`), not the type-object case.

## Repro

```
raku:  my $value = Any; say $value.Int
       # Use of uninitialized value $value of type Any in numeric context
       # 0
mutsu: my $value = Any; say $value.Int
       # No such method 'Int' for invocant of type 'Any'
```

Same for `.Num`. `Nil.Int`/`Nil.Num` reproduce identically (mutsu represents a
bare `Any` value and `Nil` the same way internally, `ValueView::Nil`).

Also reproduces through dynamic dispatch: `$value.$type-object` where
`$type-object` holds e.g. `Int` (used by DBIish's row-coercion:
`$value.$ct` in `DBDish::SQLite::StatementHandle::_row`).

## Root cause

`src/builtins/methods_0arg/dispatch_core_coerce.rs`'s `"Real"`/`"Numeric"`
handlers (~line 962 and ~1023) already implement the correct
warn-and-return-zero behavior, but only for `ValueView::Package(name)` — i.e. a
true type object like calling `.Real` directly on `Int` (the type object). A
bare `Any`/`Nil` *value* (`ValueView::Nil`) never reaches that branch, falls
through the whole native-method dispatch, and lands on "no such method".

## Why this is filed instead of fixed

Whether a value should be treated as `Nil` vs `Any` here is the same knot
documented in the (now-stale, but instructive) `project-nil-any-identity-knot`
memory: a prior dedicated deep-dive found **no clean, low-risk subset** of that
area to fix — `my $x`'s Nil default is load-bearing across the compiler
(shadow-slot/block-inline/do-expr) and VM closure-cell machinery, and touching
identity/eqv semantics there cascaded into several unrelated regressions last
time. Adding `.Int`/`.Num` support for `ValueView::Nil` specifically (not the
broader identity question) may be narrower and safer than that prior attempt,
but it still needs its own dedicated investigation into whether `ValueView::Nil`
can be distinguished from a "real" `Nil` at this call site without reintroducing
that risk — not a drive-by fix alongside unrelated work.

## Where it bit

Found via `DBIish`'s upstream `t/44-sqlite-memory.rakutest` /
`t/45-sqlite-common.rakutest` (DBIish::CommonTesting's row-typing test, which
manually forces `$sth.column-types[$_] = ...` and then relies on
`$value.$ct`-style coercion inside `DBDish::SQLite::StatementHandle::_row`).
Not DBIish-specific — any `.Int`/`.Num` call on a genuinely-undefined value hits
this.
