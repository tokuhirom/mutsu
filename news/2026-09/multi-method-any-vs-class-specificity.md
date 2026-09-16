# A `multi method` candidate typed `Any` no longer out-ranks a class-typed sibling

`multi method` resolution ranks candidates by `type_hierarchy_distance`, and
that function had two bugs that let an `Any`-typed candidate tie with, or
outright beat, a sibling candidate typed with a genuine (strict) subtype:

```raku
class Foo { has $.name; }

class User {
    multi method show(Any $a) { die "fell through to Any" }
    multi method show(Foo $a) { "Foo: " ~ $a.name }
}

say User.new.show(Foo.new(name => "hi"));
# rakudo: Foo: hi
# mutsu (before this fix): X::Multi::Ambiguous
```

1. `value_type_name()` answers the generic `"Any"` for every `Instance` value
   — it never resolves the concrete class. `type_hierarchy_distance`'s early
   `base == value_type` shortcut used that generic answer literally, so an
   `Any` constraint always scored distance 0 for *any* instance, tying with
   the instance's own class (also distance 0 via the exact-match check a few
   lines below). The shortcut is now skipped for `Instance` values, so `Any`
   falls through to the existing class-MRO walk and scores its real distance
   (the number of MRO levels between the instance's class and `Any`).

2. Across a `use`d module boundary, the class-typed constraint is often an
   env-bound alias for a lexically imported short name (`use Mod; ... Foo
   $x` really means `Mod::Foo`). The distance function compared the alias
   literally against the value's fully-qualified class name, never found a
   match, and fell back to the 500 "unrelated" distance — losing outright to
   `Any` (silently, no ambiguity error, since `Any` then had the *strictly
   lower* distance). Added the same alias-following step
   `type_matches_value` already uses to decide applicability, so the
   specificity ranking agrees with which candidate the call can actually
   reach.

The class-MRO walk itself was hardened at the same time to use
`mro_readonly` (which falls back to a live parents-only walk) instead of
reading the registry's cached `ClassDef::mro` field directly, since
`type_hierarchy_distance` takes `&self` and cannot compute-and-cache an MRO
that has not been resolved yet.

Found working
[ANTLR4::Grammar](https://github.com/tokuhirom/mutsu/issues/8566): its
`Formatting` role declares `multi method to-lines(Any $a) { die "..."}`
alongside typed candidates like `multi method to-lines(Action $a) {...}`,
and every call fell through to the `Any` candidate and died.

Pinned by `t/oo/method/multi-method-any-class-specificity.t`, covering both
the same-file case (raised `X::Multi::Ambiguous`) and the cross-module
import case (silently picked the wrong candidate), with a small fixture
module at `t/lib/Issue8566/Mod.rakumod`.
