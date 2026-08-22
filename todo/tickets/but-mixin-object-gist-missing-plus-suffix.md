# `but`-mixing a role onto a class instance: default gist drops the `+{Role}` suffix

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/Mixins.rakudoc:112`).

## Minimal repro

```raku
class Foo { }
role Bar { }

say Foo.new but Bar;     # OUTPUT: «Foo+{Bar}.new»
say Foo.new.^mixin(Bar); # OUTPUT: «Foo+{Bar}.new»
```

- `raku`: both lines print `Foo+{Bar}.new`.
- `mutsu` (`target/debug/mutsu`): both lines print `Foo.new` — the `+{Bar}` mixin
  annotation is missing.

## Root cause hypothesis

`.^name` on the same mixed value is already correct:

```raku
my $x = Foo.new but Bar;
say $x.^name;   # mutsu: Foo+{Bar}  -- matches raku
say $x.gist;     # mutsu: Foo.new    -- raku: Foo+{Bar}.new
```

So the metaclass-name path (`dispatch_caret_name` or similar) already looks at the
instance's role/mixin markers, but the default object gist/stringification path (the one
producing `TypeName.new(...)`-shaped output for a plain class instance) does not — it must
be reading the *base* type's name rather than going through the same mixin-aware name
lookup that `.^name` uses.

## Affected files (starting point)

- Wherever the default `Any`/class-instance `.gist`/`.new`-shaped stringification builds
  its type-name prefix (likely `src/runtime/methods_introspect.rs` or a gist-formatting
  helper) — needs to route through the same mixin-name resolution `.^name` already uses
  instead of the plain base-class name.
