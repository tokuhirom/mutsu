# A numeric operator on an object with no `.Numeric` answers instead of dying

rakudo's generic numeric candidates are written in terms of `.Numeric`
(`multi infix:<==>(Any \a, Any \b) { a.Numeric == b.Numeric }` and friends), so
an object whose class defines no `Numeric` method makes them fail to resolve.
mutsu answers instead.

## Repro

```raku
class Opaque { has $.v }
say Opaque.new(v => 1) == Opaque.new(v => 2);
# rakudo: Cannot resolve caller Numeric(Opaque:D: ); none of these signatures matches:
#             (Mu:U \v:: *%_)
# mutsu:  False
```

The same shape with `+`:

```raku
class Word { has $.text }
sub module-sum($a, $b) { $a + $b }   # in another compilation unit
say module-sum(Word.new(text=>'a'), Word.new(text=>'b'));
# rakudo: Cannot resolve caller Numeric(Word:D: )
# mutsu:  0
```

## Why it is not a one-liner

mutsu's numeric paths end in a structural / `to_float_value` fallback that
silently yields `False` or `0` for an object it cannot numify. Making that a
hard error is the correct behaviour but has a wide blast radius: every place a
comparison currently leans on the lenient answer would start throwing, and the
error has to be the right one (`X::Multi::NoMatch`-shaped, naming
`Numeric(<Class>:D: )`) rather than a generic message, or `throws-like`
assertions across roast will not match.

The lenient path also interacts with mutsu's bare-string enum modeling, which
deliberately keeps `==` permissive for non-numeric `Str` operands (see
`infix_is_strictly_numeric`'s doc comment) — so the fix has to distinguish
"object with no `Numeric`" from "value mutsu deliberately compares leniently".

## Where it was noticed

Writing the pin for
`news/2026-08/numeric-equality-falls-back-to-the-numeric-method.md`. The
assertion "two distinct objects with no `.Numeric` are still not equal" passes
in mutsu and *dies* in rakudo, so it could not go in a pin that must be green
under both; the pin documents the divergence in a comment and points here.

No roast file in the current `MUTSU_REAL_TEST=1` residue gates it.
