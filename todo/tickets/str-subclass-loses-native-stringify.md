# A user class `is Str` (or other native type) doesn't inherit native stringification

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1067).

## Repro

```
class Foo is Str { }
my $x = Foo.new(:value("hi"));
say $x;
say $x.Str;
say "v=$x";
```

- raku: `hi` / `hi` / `v=hi`
- mutsu: `Foo.new` / `Foo.new` / `v=Foo()`

## Root cause guess

Subclassing a native type (`is Str`, likely also `is Int`/`is Num`/etc.) is expected to give the
subclass the parent's underlying native representation and its `.Str`/stringify behavior
(reading the `$.value` the native type stores). mutsu's class system probably treats `is Str`
like any other user-class inheritance (MRO/method lookup only) without giving the instance the
native `Str` payload or wiring `.Str`/`.gist`/interpolation to fall through to it.

## Affected files (starting point)

- `src/runtime/class.rs` — class construction/inheritance from a native type
- Wherever `.Str`/interpolation/`say` resolves an `Instance`'s default stringification (likely
  falls back to `ClassName.new` formatting when there's no user `.Str`, and needs a native-type
  fallback checked first)

## Suggested next step

Check how `.new(:value(...))` is handled for a class subclassing a native type — does it store
the native payload anywhere retrievable, or does `Foo.new(:value("hi"))` just become a plain
attribute-less instance? That determines whether the fix is at construction time or at
stringify-dispatch time (or both).
