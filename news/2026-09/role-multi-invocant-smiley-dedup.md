# A role's multi candidate survives a class's same-named override with a different invocant smiley

A role's `multi method f(::?CLASS:U: Str:D $s) { ... }` candidate disappeared from a composing
class the moment that class also declared its own `multi method f(::?CLASS:D: Str:D $s) { ... }`:

```raku
role R {
    proto method f(|) { * };
    multi method f(::?CLASS:U: Str:D $s) { "U-invocant: $s" }
}
class C does R {
    multi method f(::?CLASS:D: Str:D $s) { "D-invocant: $s" }
}
C.f('x');   # rakudo: U-invocant: x   mutsu (before): Cannot resolve caller f(C:U: Str:D); ...
```

`resolve_class_stub_requirements` (`src/runtime/registration.rs`) has a legitimate rule: "when a
class provides a multi candidate matching a role candidate, the class version takes priority —
remove the role duplicate," for the case where a class re-declares an identical multi candidate
to override a role's. Whether two candidates "match" reused `method_signatures_match`, which in
turn uses `method_positional_signature` — a function whose whole point (for its *other*, original
use: deciding whether a class's concrete method satisfies a role's stub requirement) is to strip
the invocant out of the comparison entirely, since a typed invocant marker is advisory for stub
satisfaction. Reused unchanged for the "class multi replaces role multi" dedup, it made
`f(::?CLASS:U: Str:D)` and `f(::?CLASS:D: Str:D)` compare equal (both reduce to just `["Str:D"]`),
so the role's `:U:`-invocant candidate was wrongly treated as a duplicate of the class's
`:D:`-invocant one and dropped — even though the two are genuinely different, coexisting multi
dispatch candidates in Raku.

Added `method_signatures_match_with_invocant`, used only at this one dedup site, which additionally
requires the invocant's own type constraint to match. The stub-satisfaction use of
`method_signatures_match` (unrelated to this bug) is untouched. A class multi that genuinely
re-declares the *same* invocant smiley as the role's still correctly replaces it — no duplicate
candidate is left behind.

This was blocking `TAP` (`t/source-file.rakutest`): `TAP::SourceHandler`'s
`multi method make-source(::?CLASS:U: Str:D $name, *%args) { self.new.make-source($name, |%args) }`
re-dispatch pattern needs exactly this shape, and `SourceHandler::File`'s own `:D:`-invocant
override was silently eating the role's `:U:` candidate.

Fixes #8119.
