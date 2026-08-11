# ADR-0019 E5b step 1: the Native candidate is not yet safe to route CallMethod's dispatch decision

With all four E5 measurement slices done, this step began E5b (the
`CallMethod` cutover to the E4 resolver decision) the same way every prior
Phase E box has: shadow-verify before switching. It reused the existing E4b
step 9 shadow-check function unmodified at `CallMethod`'s own
highest-traffic plain-probe arm, comparing design decision 4's `Native`
candidate against what `try_native_method` actually did — pure insertion,
zero behavior change.

The result is a real finding, not a clean bill of health: a full `t/` sweep
found ~965 mismatches out of 39558 checks (~2.4%), spread across 253 files,
in both directions and with no single dominant method. This directly
contradicts E4b step 9's earlier report of essentially zero mismatches for
the *same* shadow-check function — but that check ran at a much
lower-traffic call site (the interpreter's slow path), so its clean result
was a sampling artifact of where it was placed, not evidence the underlying
`native_row_servable` predicate actually holds across `CallMethod`'s real
traffic. Two concrete root causes: the predicate is blind to
concrete-value-shape exceptions (`Sub.gist`/`.raku` decline a generic row
that claims to cover them), and some methods the cascade genuinely serves
(`DEFINITE` at 0 arity) have no catalog row at all.

Consequence: E5b's planned "native or user" branch cannot be built purely
from the `Native` candidate as originally sketched — it would silently
mis-route roughly one in forty calls on this arm. The fix is either a
per-shape refinement of the predicate, or keeping the actual dispatch as a
direct, self-guarding `try_native_method` call rather than a resolver
decision that skips calling it (mirroring how `NativeCallBinding` was
already found not worth routing through the resolver at E4b step 12). This
is exactly the kind of correctness risk the project's shadow-verify-first
methodology exists to catch before a large cutover PR, not after.

Full detail, mismatch examples, and the open next question (whether
`CallMethod`'s existing `skip_native` gate already makes this moot for this
one entry) are in `todo/deep/adr0019-e5-e7-entry-routing.md` §"E5b step 1"
and `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`
(E5 bullet).
