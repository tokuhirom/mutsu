# A plain (non-`Cool`) class answers `Cool`-only builtin methods it should not have

Reclassified from `todo/tickets/plain-classes-answer-cool-only-builtin-methods.md`
(2026-08-18) after attempting the fix that ticket sketched and finding the
blast radius is real and larger than "verify it's not common, then land it" —
see "What was tried" below for the empirical measurement.

## The bug

```raku
class G {}
say G.new.uc;   # raku: dies "No such method 'uc' for invocant of type 'G'"
                # mutsu: "G()"
```

A plain class derives from `Any`/`Mu`, not `Cool` — `.uc`, `.flip`, `.subst`,
`.trans`, and the other `Cool`-only builtins (the set in
`Interpreter::cool_only_builtin_method`, `src/runtime/methods_native_bypass.rs`)
are simply not in `G`'s MRO. mutsu instead answers this set for **any**
Instance receiver regardless of whether its MRO actually includes `Cool` — the
native fast path (`try_native_method_raw` in `src/vm/vm_native_dispatch.rs`,
and the by-name dispatcher in `methods_call_dispatch.rs`) stringifies the
receiver and applies the builtin unconditionally.

## What was tried (2026-08-18): a straightforward MRO gate, and why it regressed

The obvious fix: extend the existing `handles */FALLBACK`-only gate (added by
`todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md`'s fix) to
apply unconditionally. Three call sites already share the same shape —
`should_bypass_native_fastpath` (`methods_native_bypass.rs`),
`shadows_builtin` (`methods_call_dispatch.rs`), and `try_native_method_raw`
(`vm_native_dispatch.rs`) — each with:

```rust
|| (Self::cool_only_builtin_method(method)
    && self.class_has_wildcard_handles_or_fallback(&class_name))
```

Changing the right-hand condition to `!self.class_mro_includes_cool(&class_name)`
(a new helper: `self.class_mro(class_name).iter().any(|cn| cn.as_str() == "Cool")`,
confirmed `class_mro` already correctly excludes `Cool` for a plain class and
correctly includes it for `class H is Cool {}` — so the MRO data itself is not
the gap) makes the repro above die correctly, and flips
`t/handles-wildcard-builtin-methods.t`'s two long-standing `todo`-marked
assertions to genuinely passing (confirmed via `TODO passed: 14-15`).

**But a full local `make test` run surfaced 6 new failures**, none of them
involving a plain user class at all — every one is a mutsu-*builtin* type
whose class registration doesn't include `Cool` in its MRO (correctly,
matching real Rakudo — verified `raku -e 'say DateTime.new(...).^mro'` is
ALSO `((DateTime) (Any) (Mu))`, no `Cool`) but which genuinely implements a
same-named method **directly**, independent of Cool inheritance:

| File | Failing call | Rakudo `.can(...)` |
|---|---|---|
| `t/date-format-methods.t`, `t/str-date-coercion.t`, `t/http-deps-battery.t` | `DateTime.Date` | `DateTime.now.can("Date")` → `(Date)` (own method, not via Cool) |
| `t/instant-duration-do-real.t` | `Instant.abs` | own method |
| `t/native-instant-from-posix.t` | `Instant.DateTime` | own method |
| `t/metamodel-and-coercion-gaps.t` | `Date.IO` | own method |
| `t/constant.t` | `IO::Path.chars` | own method |

So `cool_only_builtin_method`'s NAME LIST (`.Date`, `.abs`, `.DateTime`,
`.IO`, `.chars`, ...) conflates two genuinely different situations that
happen to share a name:

1. **Truly Cool-only for a generic Instance** — `.uc`/`.flip`/`.subst`/... on
   an arbitrary user class: no per-type implementation exists, the ONLY
   reason the fast path answers is a generic "stringify the receiver and
   apply the Cool-family string op" catch-all that doesn't consult the
   class's actual MRO at all. This is the bug.
2. **Coincidentally-named genuine per-type native methods** — `DateTime.Date`,
   `Instant.abs`, `Date.IO`, `IO::Path.chars`, etc. are real, class-specific
   implementations (mutsu's native builtin dispatcher, e.g.
   `native_method_0arg`, checks the receiver's actual type internally and
   only answers for that specific type) that happen to share a name with the
   Cool-only set, but have NOTHING to do with Cool inheritance — they would
   exist on these types even if Cool never existed. Blocking these because
   "the class's MRO lacks Cool" is simply wrong; Cool-ancestry was never how
   these resolve in the first place, on mutsu OR on real Rakudo.

**A first attempt to fix category 2 by also excluding `is_native_method(&class_name, method)` did not work**: `is_native_method` checks a completely
different, narrower registry (`registry().classes[cn].native_methods`, a
per-class-registration set populated by a separate mechanism) than what
actually answers `DateTime.Date` — that answer comes from
`src/builtins/native_method_row_table.rs`'s `RAW_ROWS` table (confirmed a
`("DateTime", "Date", 1, 0)` row exists there) feeding
`native_method_0arg`/`native_method_1arg`/`native_method_2arg`'s dispatch,
which `is_native_method` does not consult at all. `RAW_ROWS` is
`pub(super)` (only visible inside `builtins`), so there is no existing
cross-module "does this (class, method) pair have a genuine row-table
answer" predicate to reuse — one would need to be built, exposed, and
threaded into all three gate sites, OR the gate would need to be
restructured entirely (e.g. try the native dispatch unconditionally, and
only enforce the Cool-only-die rule if it returns `None`) — a real design
question, not a one-line follow-up.

## Why this is `todo/deep`

- The three gate sites (`should_bypass_native_fastpath`, `shadows_builtin`,
  `try_native_method_raw`) need a NEW, correctly-scoped predicate — "does
  this exact (class, method) pair have a genuine per-type native
  implementation, independent of the generic Cool catch-all" — that doesn't
  exist yet and isn't a trivial derivation from data already exposed across
  module boundaries (`RAW_ROWS` is private to `builtins`).
- The list of affected coincidentally-named methods is not fully enumerated —
  only 6 files' worth were found via one `make test` run; a careful audit of
  every name in `cool_only_builtin_method`'s set against every builtin type
  that might implement it directly (Date, DateTime, Instant, Duration,
  IO::Path, and potentially others not exercised by the current `t/`/`roast/`
  suite) is needed before landing a general fix, or more regressions will
  surface exactly the way these six did — this session's whole point was
  "measure before assuming," and the measurement showed real, non-trivial
  scope.
- The original ticket's own "Why this is a separate, larger ticket" section
  correctly anticipated the shape of this risk in the abstract ("(a) genuinely
  have Cool composed... or (b) start dying where it previously silently
  worked"); this investigation adds the CONCRETE list and the CONCRETE reason
  the naive fix doesn't work, which is the missing piece for whoever picks
  this up next.

## Suggested fix direction (not attempted further this session)

Restructure the three gate sites so a `cool_only_builtin_method` name is
allowed through the ORDINARY native dispatch attempt first — the SAME
`native_method_0arg`/`_1arg`/`_2arg` call each site's caller already makes —
and only the FINAL failure to resolve at all (both native dispatch returning
`None` AND no user method/accessor) triggers a "no such method"-style error
instead of falling through to the wrong generic catch-all. This sidesteps
needing to classify each name/type pair ahead of time: `DateTime.Date`
"naturally" continues to resolve because the DateTime-specific row answers
it; `G.new.uc` "naturally" fails because nothing in `native_method_0arg`'s
dispatch answers `uc` for an arbitrary `Instance` once the generic
stringify-then-uc catch-all is EITHER removed OR itself gated on Cool
ancestry (the real fix likely lives in that one generic catch-all arm inside
`native_method_0arg`, not in the three call-site gates at all — worth
checking whether narrowing THAT one arm is sufficient before touching the
three gates).

## Severity

Low: a missing compile/runtime-time diagnostic (mutsu is too lenient), not a
miscompilation — every genuine Cool-derived or native-type call still answers
correctly today. No roast test currently depends on `G.new.uc` dying.

Affected: `src/runtime/methods_native_bypass.rs` (`cool_only_builtin_method`,
`should_bypass_native_fastpath`), `src/runtime/methods_call_dispatch.rs`
(`shadows_builtin`), `src/vm/vm_native_dispatch.rs` (`try_native_method_raw`),
`src/builtins/methods_0arg/` (wherever the generic Cool-family
stringify-fallback arm lives — the likely real fix site),
`src/builtins/native_method_row_table.rs` (`RAW_ROWS` — would need a public
membership-check helper if the three-gate-site approach is taken instead).
