# `::?ROLE:D` invocant type + nested colonpair-alias named param breaks method registration

Found during the XML battery survey (`docs/batteries/xml.md`) while investigating why
`LibXML` (`zef:dwarring`, the libxml2 NativeCall binding) fails at `use LibXML;` itself
on mutsu.

## Root cause

`LibXML::_Configurable` (`lib/LibXML/_Configurable.rakumod` in the `LibXML` dist) has:

```raku
unit role LibXML::_Configurable;
...
multi method create(::?ROLE:D :from(:$for)! is raw, |c) {
    self.WHAT.new: :config($for.config), |c
}
```

`::?ROLE:D` is the role's own meta-invocant-type variable (analogous to `::?CLASS:D`
for classes); `:from(:$for)!` is a **nested colonpair parameter alias** — a required
named parameter callable as either `:from(...)` or `:for(...)`, bound to `$for`
internally (documented idiom, `raku-doc/doc/Language/signatures.rakudoc`).

On mutsu, declaring a role method whose invocant type is `::?ROLE:D` and which also has
a nested-colonpair-alias named parameter fails with:

```
Invalid typename 'from' in parameter declaration.
```

This happens at role-body evaluation time (i.e. merely declaring the role, no
composition or call needed) and — critically — **the parse error is swallowed rather
than propagated**: the file continues running with the method simply absent from the
role (see the companion ticket
`todo/tickets/indirect-type-param-parse-failure-silently-drops-role-method.md` for the
general "silently dropped role method" shape, which is the same failure mode as this
bug's symptom once composed into a class).

## Minimal repro

```raku
role Foo {
    method create(::?ROLE:D :from(:$for)!) {
        say $for;
    }
}
say "loaded ok";
```

- `raku`: prints `loaded ok` (parses and composes fine; `Foo` is a role so nothing
  calls `create` here, but declaration succeeds).
- `mutsu` (`target/debug/mutsu`): fails with
  `Invalid typename 'from' in parameter declaration.` before reaching the `say`.

### Isolating the two ingredients

Each ingredient alone is fine on mutsu; only the combination breaks:

```raku
# OK on mutsu: ::?ROLE:D invocant + a PLAIN named param
role Foo {
    method create(::?ROLE:D :$for!) { say $for }
}
say "loaded ok";      # -> loaded ok

# OK on mutsu: nested colonpair alias + a CONCRETE class invocant (not ::?ROLE)
class Foo {
    method create(Foo:D :from(:$for)! is raw) { say $for }
}
say "loaded ok";      # -> loaded ok (also OK with ::?CLASS:D in place of Foo:D)
```

So the bug is specifically the pairing of the role's own meta-invocant-type variable
(`::?ROLE:D`) with a nested colonpair-alias named parameter in the same signature.

## Why this matters beyond LibXML

This blocks `use LibXML;` entirely — the module never loads on mutsu, so its full
70-file / 723-test upstream suite (100% green under `raku`) cannot even start. See
`docs/batteries/xml.md` for the full survey; `LibXML` is otherwise the strongest
NativeCall-based candidate in the field (Artistic-2.0, actively maintained — last push
2026-06-10 — 7 dependents, and the machine already has libxml2 + its dev headers
installed so the native shim builds cleanly).

## Affected files (starting point, not exhaustive)

- Wherever role method signatures are parsed/registered — likely the same signature
  parsing path exercised by `todo/tickets/grammar-token-param-dynvar-not-visible-in-subrule.md`'s
  neighborhood is unrelated; this one is specifically about `::?ROLE` meta-type
  resolution interacting with the nested-colonpair-alias parameter parse, probably in
  `src/parser/` (signature/parameter grammar) or `src/compiler/` (role method
  registration). Worth checking whether `::?CLASS` invocants share the same code path
  as `::?ROLE` — the repro above shows `::?CLASS:D` does NOT trigger the bug, so the
  role-specific meta-type resolution is the more likely culprit.

Not root-caused further within this survey's time budget.
