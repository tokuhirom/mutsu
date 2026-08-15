# ADR-0019 Phase F box F1: `.^lookup`/`.^find_method` now return a real `Method`/`Submethod` Instance

`.^lookup`/`.^find_method` used to return a `Sub`-shaped (or, for native methods, `Routine`-shaped)
value — a different representation from the `Method` `Instance` `.^methods`/`.^method_table`/`.^can`
build via `make_method_object_with_owner`/`make_native_method_object`. Any `Method`-only accessor
(`.is_dispatcher`, `.multi`, `.rw`/`.readonly`, `.candidates`) was unreachable on a `.^lookup` result:
calling an unrecognized method on a `Sub` falls into the "method calls on callables compose" fallback,
which silently returns a bogus `<composed-method:NAME>` callable instead of a real answer or an error
(`todo/tickets/classhow-lookup-returns-sub-not-method-instance.md`, opened while scoping F1/F2, and
partially patched by #6420 with `is_dispatcher`/`.multi` env-tag special cases on the `Sub` shape).

## The fix: unify on the `Instance` shape, keep it callable via `CALL-ME`

`classhow_lookup_impl` (`.^lookup`'s and `.^find_method`'s shared implementation) now builds the same
`Method`/`Submethod` `Instance` `.^methods` does, for all four cases it used to special-case: a plain
user-class method, a role method, an auto-generated attribute accessor, and a native/builtin method
(including grammar tokens, which now go through `make_native_method_object_ex`'s new `is_regex` flag).
Direct callability (`$m(invocant, args)`, which real Raku supports as `Method`'s implicit `CALL-ME`) is
preserved by attaching the original callable `Sub`/`Routine` as a hidden `__mutsu_method_callable`
attribute on a non-dispatcher `Instance`; a new `CALL-ME` handler in `methods_instance_ops.rs` either
invokes that attribute directly or, for a multi dispatcher (which has no single callable of its own),
re-dispatches on the first argument as invocant — mirroring the old Sub-shaped dispatcher's
`sub_multi_method_dispatcher_name` re-dispatch. `make_method_object_with_owner`/`_ex` gained two more
generically-correct attributes needed for parity: `.multi` (previously missing entirely from the
`Instance` shape) and `.rw`/`.readonly`.

As a side effect this also fixes `.^methods()[N](...)` — the SAME `CALL-ME` gap on the `Instance` shape
`.^methods` already returned, found while verifying the unification didn't regress that surface.

## Real bugs found and fixed along the way, not just plumbing

- **`Method.signature` was missing the invocant.** `make_method_object_with_owner`'s `.signature`
  never prepended an invocant param (`param_defs_to_sig_info(&method_def.param_defs, ...)` used the
  declared params only) — real Rakudo's `Method.signature` always carries the invocant as `params[0]`
  (`B.^find_method('foo').signature.gist` is `(B $:: $!a, *%_)`, not `($!a, *%_)`). This was a
  pre-existing gap in the `.^methods` path (silently never exercised until `.^lookup` started routing
  through the same builder) that broke `roast/S06-signature/introspection.t`'s private/public attribute
  Parameter twigil checks (`params[1]` was off by one without the invocant). Fixed by prepending one the
  same way the attached callable's own params already do.
- **`.candidates` for a plain (non-multi) method.** Real Raku: a non-multi method's own `.candidates` is
  itself, a one-element list (`Foo.^lookup('bar').candidates[0]` on a plain method must work, not just a
  multi's) — `make_method_object_with_owner` only populated `.candidates` for a dispatcher. Fixed by
  defaulting a non-dispatcher's `.candidates` to `[self]`.
- **A multi family spanning several classes in the MRO.** `.^lookup`'s dispatcher for a multi method
  must expose the FULL combined candidate family across ancestors (`C1`/`C2` each contributing `bar`
  candidates, `roast/S06-advanced/wrap.t`'s "multi methods with a wrapped one are in order"), unlike
  `.^methods`'s intentionally per-class-only `.^method_table` view. `classhow_lookup_all_candidates` (the
  existing MRO-combining walk) was switched to build `Instance`-shaped candidates too, and a pre-existing
  bug in the single-owner candidate builder (every multi candidate's `__mutsu_lookup_candidate_idx`
  hardcoded to `0`, so `.wrap()`-ing `.candidates[1]` collided with `.candidates[0]`'s wrap slot) was
  fixed by threading the real per-owner index through.
- **`.WHY` (declarator doc comments) on a `Method`/`Submethod` `Instance`.** The generic
  `ValueView::Instance` branch of `dispatch_why` had no case for these, falling through to a nonsense
  `class_name.resolve()` ("Method") lookup key. Added a branch building the same
  `"{owner}::{method}"`/`"&{method}"` keys the old `Sub` branch did, from the `__mutsu_lookup_class`/
  `__mutsu_lookup_method` attributes `.wrap` already reads (`roast/integration/advent2011-day10.t`).
- **Dynamic hyper dispatch on a method VALUE (`».$var`, `>>.&$var`).** `exec_hyper_method_call_dynamic_op`
  decided "is this a callable or a plain method-name string" (and, for a callable, its "nodal" name for
  `».+`/`».*` descent) by pattern-matching `ValueView::Sub | WeakSub | Routine` directly — a `Method`
  `Instance` fell through to the string-name path instead, losing the nodal-callable check
  (`roast/S03-metaops/hyper.t`). Added a small `method_value_callable_name` helper recognizing the
  `Instance` shape too.
- **`^add_method`/`^add_multi_method`'s multi-family alias detection.** These read
  `__mutsu_lookup_class`/`__mutsu_lookup_method`/`__mutsu_lookup_candidate_idx` off the callable
  argument's `SubData::env` to detect "this carries a whole multi family, not just one candidate"
  (`^add_method(name, X.^lookup('other'))` must alias every candidate, not freeze to the first one's
  signature — Text::CSV's BEGIN-time `alias` helper does exactly this). Those tags now live as
  `Instance` attributes, not `Sub` env, so a new `unwrap_method_instance_callable` ports them back onto
  the unwrapped `Sub`'s env before the existing detection logic runs (falling back to the dispatcher's
  first candidate as the carrier body when the `Instance` itself has no direct callable), pinned by
  `t/addmethod-multi-alias.t` and `t/can-multi-dispatcher.t`.

## Verification

New pin: `t/classhow-lookup-method-instance-callable.t` (9 assertions, byte-for-byte matched against
`raku`). Existing pins stayed green throughout (`t/classhow-lookup-method-is-dispatcher-multi.t`,
`t/wrap-candidate-unwrap-restore.t`, `t/can-multi-dispatcher.t`, `t/addmethod-multi-alias.t`, and the
whole `t/wrap-*`/`t/classhow-*` family). Full local `t/` suite (3171 files) green. Targeted roast sweep
covering every method-dispatch/introspection/multi/wrap/hyper/role directory (`S02`, `S03-metaops`,
`S06-*`, `S12-*`, `S14-*`, `S17-*`, `integration`) found and fixed four real regressions during
development (`roast/S12-attributes/instance.t`'s `.rw` accessor check, `roast/S06-signature/
introspection.t`'s twigil indices, `roast/integration/advent2011-day10.t`'s `.WHY`, `roast/S03-metaops/
hyper.t`'s nodal dynamic-hyper dispatch, `roast/S06-advanced/wrap.t`'s cross-class multi candidates) —
all confirmed fixed, and every other failure in that sweep (`S12-attributes/trusts.t`, `S12-class/
open_closed.t`, `S02-types/quanthash.t`, `S06-advanced/caller.t`, `S06-advanced/return_function.t`,
`S12-meta/exporthow.t`, `S12-traits/basic.t`, `S12-traits/parameterized.t`) verified pre-existing on
`main` via `git stash`, none of them whitelisted.

## What remains open

The two blockers the original ticket named turned out smaller than feared: `.wrap`'s tag reuse needed
no change at all (the `Instance` shape already carried the same `__mutsu_lookup_*` tags `.^methods(:
local)` used), and direct callability needed one `CALL-ME` handler at the single centralized
"invoke a Value" entry point (`call_sub_value`/`dispatch_instance_and_fallback`), not a general
"make an Instance callable everywhere" capability. The F1 *fidelity* slice (per-native-method
`.signature`/`.package`/`.is_dispatcher` override columns on `NativeMethodRow`, populated reactively —
see `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`'s "Decision (2026-08-14)") remains
correctly idle until a real `t/`/roast assertion demands a specific override.
