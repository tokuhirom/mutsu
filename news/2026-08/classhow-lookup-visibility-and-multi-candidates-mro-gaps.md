# `.^lookup`/`.^find_method` no longer surface a private method, and `.candidates` no longer misses an inherited multi family

Two related gaps in `src/runtime/methods_classhow_lookup.rs` (both found
while investigating ADR-0019 E7 step 5's MRO-walk fix, and left as separate
tickets since they are different bug shapes):

## 1. `.^lookup(name)` used to find a private method

```raku
class A { method !secret { "shh" } }
say A.^lookup("secret").defined;
```

raku: `False` — `.^lookup` never surfaces a private method by its bare
(unqualified, no `!`) name, even from inside the declaring class itself.
mutsu answered `True`.

`classhow_lookup`'s per-level MRO walk (`class_def.methods.get(method_name)`
via `user_method_overloads`) never checked `def.is_private` before building
and returning a method object for the match — every other visibility-aware
dispatch path in the codebase (`resolve_method_with_owner_impl`'s `Public`
filtering, `resolve_sequence`'s `MethodVisibility::Public` tier) already
skips `is_private` defs. Fixed by filtering each MRO level's candidate defs
to non-private before taking the first one, continuing to the next level
(rather than stopping) when a level's only match is private.

## 2. `.^find_method(name).candidates` missed an inherited multi method family

```raku
class A {
    multi method foo(Int $x) { "A::foo(Int)" }
    multi method foo(Str $x) { "A::foo(Str)" }
}
class B is A {}
say B.^find_method("foo").candidates.elems;   # raku: 2, mutsu (before): error
```

`classhow_lookup_all_candidates` decided whether the resolved method was
`multi` by checking the *receiver's own class* (`class_method_is_multi(class_name)`
where `class_name` was always `B`, never `A`), then — for the (wrongly
selected) non-multi branch — always used `vec![class_name.to_string()]` as
the sole owner instead of walking the MRO to find which class actually
declares the method. For an inherited-only multi family, both mistakes
compounded: the multi-ness check missed it (B has no own `foo`), so it fell
into the non-multi branch, which then looked up a nonexistent `B::foo` and
returned an empty candidate list — surfacing to a caller as "No such method
'candidates'" rather than the two inherited candidates.

Fixed by first walking the MRO (most-derived first) to find the actual
owning class, then deciding multi-ness — and building the non-multi owner
list — from that owning class instead of the receiver's own class.

Both are pinned in `t/classhow-lookup-mro.t` (extended from 14 to 17
assertions, byte-identical against `raku`).
