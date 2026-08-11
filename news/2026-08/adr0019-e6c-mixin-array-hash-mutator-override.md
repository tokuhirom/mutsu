# ADR-0019 E6c closes: a `does`-mixed role's `push`/`unshift`/`append` was silently shadowed by the native array/hash mutator, on both static and dynamic-name mut dispatch

E6's design doc flagged two "inventory correction" gaps left unverified from the E5/E6
entry survey: item 4 (`HyperMethodCallDynamic` missing the `skip_native` gate its static
twin has) and item 3 (`CallMethodDynamicMut` reaching the interpreter with no native or
compiled-method probe at all).

Item 4 turned out to already be closed. E5c's own raku-verification pass (three targeted
collision attempts — an Instance method override, a `but`-mixin string override) found
`try_native_method_raw`'s internal guards are the real safety net regardless of whether
the caller precomputes an outer gate; `HyperMethodCall`'s own gate is a fast-path bypass,
not a distinct correctness mechanism. Nothing left to do.

Item 3 raku-verified real:

```raku
role Loud { method push($x) { say "ROLE-PUSH: $x"; self } }
my @a = (1, 2, 3);
@a does Loud;
my $name = "push";
@a."$name"(4);
say @a;
# raku:  ROLE-PUSH: 4 / [1 2 3]
# mutsu (pre-fix): [4]  — the role method never ran
```

Tracing where the wrong output came from (rather than patching the opcode on
suspicion) found the bug one level deeper than the box's own framing suggested.
`CallMethodDynamicMut`'s generic fork and `CallMethodMut`'s own generic fork (reached for
any array/hash mutator without a dedicated fast opcode — `ArrayPush` is the *only*
mutator with one) both bottom out in the same function,
`call_method_mut_with_values`. That function special-cased
`push`/`append`/`unshift`/`prepend`/`pop`/`shift`/`splice` purely by **sigil**
(`target_var.starts_with('@')` / `('%')`), with no check that the value behind the sigil
was still a plain `Array`/`Hash` and not a `does`-mixed `Mixin` — unlike every other
native-vs-user gate this campaign has touched (`ArrayPush`'s own `is_simple_array` guard
from E6d, the Tier-A `try_native_array_mut` helper, and `try_native_method_raw`'s own
`mixin_role_has_method` bypass, the exact mechanism item 4 leans on). So the "dynamic
gap" and this deeper slow-path gap were the same bug wearing two faces — and it was
reachable from the *static* path too:

```raku
role Loud { method unshift($x) { say "ROLE-UNSHIFT: $x"; self } }
my @a = (1, 2, 3);
@a does Loud;
@a.unshift(4);   # raku: ROLE-UNSHIFT: 4 / [1 2 3] — mutsu (pre-fix): [4]
```

Fixed by gating both the array-mutator and hash push/append blocks with
`!self.mixin_role_has_method(&target, method)` — the identical guard
`try_native_method_raw` already applies — so a mixin-role method for the called name
falls through to the function's own existing generic tail instead of the native
fast-mutator code. The fourth time this campaign has found "the receiver-shape check the
fast path already has to make IS the safety net" (after `CallMethod`'s native probe,
the augmented native collection methods fix, and `ArrayPush`'s own guard).

Pinned as `t/mixin-array-hash-mutator-override.t` (8 assertions, raku-verified
byte-identical). Full `t/` suite green; a 190-file roast slice across role/mixin/array/
hash-relevant synopses clean except two pre-existing, unrelated failures
(`S02-types/quanthash.t`, `S12-attributes/trusts.t`) confirmed to reproduce identically
with this change reverted.

E6c is closed. All of ADR-0019 Phase E's E6 (E6a, E6b, E6c, E6d) is now closed; next up
is E7 (metaobject, qualified, and re-entrant calls).
