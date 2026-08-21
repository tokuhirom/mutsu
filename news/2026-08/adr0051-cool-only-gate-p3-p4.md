# A plain class no longer answers Cool-only builtin methods it doesn't have

```raku
class G {}
say G.new.uc;   # raku: dies "No such method 'uc' for invocant of type 'G'"
                # mutsu (before this fix): "G()"
```

Landed ADR-0051 ([`docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md`](../../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md))
phases P3 and P4, on top of P1 (already landed, 2026-08-20). This retires
`todo/deep/plain-classes-answer-cool-only-builtin-methods.md`: the reverted
2026-08-18 attempt recorded there is now understood and fixed for real.

## P3 — filled the missing native-method rows

Added the two rows the ADR's own measurement named as genuinely missing
(`("Instant", "DateTime", 1, 0)` and `("Date", "IO", 8, 12)`), then audited
the rest of `cool_only_builtin_method`'s 94-name list against the seven types
P1 touched (`Instant`, `Duration`, `IO::Path`, `IO::Handle`, `DateTime`,
`Date`, `Match`), raku-verified name by name (`Cool.^can(name)` in real
`raku`, plus direct `TYPE.^can(name)` sweeps).

That audit found the gap was much larger than "two rows": 48 more names in
the list genuinely resolve via `Cool` in real Rakudo but had no `"Cool"` row
at all in `RAW_ROWS` (`native_method_row_table.rs`) — e.g. `tclc`, `sin`,
`cos`, `NFC`, `IO`, `Version`, the whole trig/Unicode/coercion tail. Left
unfilled, P4's existence gate would have wrongly rejected genuine calls like
`Instant.sin` or `Match.NFC` on the four types P1 gave real `Cool` ancestry.
Six cool_only names (`Date`, `DateTime`, `lazy`, `race`, `hyper`,
`parse-base`) were deliberately *not* added as `Cool` rows — real Rakudo's
`Cool.^can` is 0 for all six; they resolve through some other, narrower path
even though a plain class also lacks them, so a `Cool` row for them would
have been a false claim, not a fix.

The audit also turned up six more genuine own-method rows missing entirely
(all already dispatched correctly, just invisible to `.^can`/existence
checks): `Instant.Date`, `DateTime.IO`, `DateTime.DateTime`, `Date.pred`,
`Date.Date`, `Date.DateTime`.

Some pre-existing false positives surfaced during the audit that are out of
this phase's scope (P2's job — collapsing/reconciling the ancestry tables,
not adding rows): `Match.^can` wrongly answers `succ`/`pred`/`base`/
`polymod`/`parse-base` as `1` via a pre-existing (inaccurate) `Cool` row or a
stray `Match`-specific row, when real Rakudo says `0`. `Any.lazy` was
already a similar false positive before this PR. None of these are
introduced or worsened by P3/P4 — they were already reachable via
`e2_native_method_exists` before this PR, just via `.^can` rather than a
dispatch gate.

## P4 — gated the string-coercion leak

At the three gate sites (`should_bypass_native_fastpath`,
`shadows_builtin`, `try_native_method_raw`) and the two by-name dispatchers
(`.IO`, `.subst`), a `cool_only_builtin_method` name on an `Instance`
receiver now additionally requires `Interpreter::e2_native_method_exists` to
say the method genuinely exists somewhere in the receiver's dispatch chain
before the fast-path/interceptor machinery is allowed to answer it. When it
doesn't, the call now falls through to ordinary "no candidate found"
resolution, which already throws `X::Method::NotFound` with byte-identical
text to real Rakudo — no new error path needed.

`class G {}; G.new.uc` now dies exactly like real `raku`. Every genuine
Cool-derived and native-type call (`Instant.abs`, `IO::Path.chars`,
`DateTime.Date`, `Instant.DateTime`, `Date.IO`, ...) still resolves
correctly — pinned by `t/adr0051-cool-only-gate.t`. The two long-`todo`-marked
assertions in `t/handles-wildcard-builtin-methods.t` (14-15) now genuinely
pass.

`cool_only_builtin_method`'s `handles */FALLBACK` term was left untouched
per the ADR — it answers a different question ("may an interceptor see this
call") from the new existence check.

P2 (collapsing the remaining ancestry tables onto one oracle) and P5
(retiring `cool_only_builtin_method` once P4 is authoritative) remain not
started.
