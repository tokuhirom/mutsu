# `handles *` (wildcard delegation) now beats built-in Cool/Any methods

`handles *` on a method or attribute lets a class intercept any method call
not otherwise defined and forward it to the delegate. In `raku`, this wins
even over a built-in method the object would otherwise inherit from
`Cool`/`Any` (e.g. `.uc`):

```raku
class Forward {
    method inner() handles * { 'hello' }
}
say Forward.new.uc;   # raku: HELLO (forwarded to 'hello'.uc)
```

mutsu previously resolved `.uc` through the native builtin fast path before
ever reaching the wildcard-delegation fallback, so `Forward.new.uc` returned
the upper-cased default stringification of the instance (`FORWARD()`)
instead of forwarding.

Root cause: the native fast path answers any *Cool*-only builtin method
(`.uc`, `.flip`, `.subst`, ...) for **any** Instance receiver unconditionally
— it has no concept of "does this class actually inherit `Cool`", let alone
"does this class declare `handles *` / `FALLBACK`, which raku gives a chance
to intercept before falling back to that error". Fixed with a name-gated
check (`Interpreter::cool_only_builtin_method`, an oracle-verified set of
~90 builtin names that only resolve via `Cool`) plus a class predicate
(`class_has_wildcard_handles_or_fallback`, walking the MRO for any
`wildcard_handles` entry or a `FALLBACK` method) wired into the three
dispatch gates that could answer a Cool-only name before the interpreter's
wildcard/`FALLBACK` fallback block ever ran: the VM's
`try_native_method_raw` (`vm_native_dispatch.rs`), the interpreter's
`should_bypass_native_fastpath` (`methods_native_bypass.rs`), and the
by-name dispatcher's `shadows_builtin` gate (`methods_call_dispatch.rs` —
needed for n-arg builtins like `.subst`, which route through a separate
table from the 0-arg ones).

Also fixed a second, independent bug found while tracing this: method-based
wildcard delegation (`method inner() handles * { ... }`) was entirely
broken even for **non-builtin** method names — the fallback block only knew
how to resolve an *attribute*-based delegate (`has $.t handles * = ...`), so
a method-based delegate's `"&inner"` marker was never recognized. Fixed by
mirroring `forward_resolved_delegation`'s existing `&`-prefix handling for
the explicit-`handles <list>` form.

New test: `t/handles-wildcard-builtin-methods.t` (17 assertions, 15 passing
— the remaining 2 are `todo`-marked pending a separate, broader gap found in
the process: plain (non-`Cool`) classes wrongly answer Cool-only builtin
methods too, see `todo/tickets/plain-classes-answer-cool-only-builtin-methods.md`).

Full `t/` suite (3184 files) clean; verified the explicit regression
hazards this investigation flagged — `roast/S12-attributes/delegation.t`,
`roast/S12-methods/delegation.t`, and 134 whitelisted `S12`/`S02-types`/
`S03-operators` files on release, plus `t/attr-handles-angle-word.t`,
`t/handles-paren-strings.t`, `t/augment-method-handles-forwarder.t`,
`t/version.t`, `t/regex-my-var-interpolation.t` — all pass.
