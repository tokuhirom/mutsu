# A closure's read-only captured scalar is hijacked by a same-named lexical in the CALLER's env chain (lexical scoping degrades to dynamic scoping)

## TL;DR — root cause is now fully diagnosed

The closure-call captured-env merge in
`src/vm/vm_closure_dispatch.rs` (`for (k, v) in data.env.iter()`) installs
non-`ContainerRef` captured values with `entry_or_insert_sym` — **don't
overwrite** — into a frame env that is a `scoped_child` of the **caller's**
env. So when a closure is invoked from a frame whose env chain happens to
contain a same-named variable, the caller's value shadows the closure's own
lexical capture. The three protections that normally paper over this all
have holes for the "creator wrote the variable before creating the closure,
closure only reads it" shape:

- `box_captured_lexicals` (lever C) boxes only captured-**and-mutated**
  locals into shared `ContainerRef` cells (a cell wins the merge via the
  overwrite branch). A read-only capture gets no cell.
- `cc.authoritative_free_vars` (overwrite-install) excludes any lexical the
  creating frame **ever writes** — so `my $encoder; ... $encoder = X.new;`
  (declare-then-assign, the most common mainline pattern) is not
  authoritative, even though every write precedes the closure's creation.
- `data.owned_captures` covers per-iteration loop captures only.

The don't-overwrite default exists deliberately: mutsu captures by value,
so a creator that mutates AFTER capture (`my $s = 0; @cb.push({ $s });
$s = 42`) needs the closure to read the LIVE value through the caller
chain. That mechanism cannot distinguish "creator's live binding" from
"unrelated same-named lexical of whatever frame happens to be calling" —
which is exactly the failure here.

## Real-world failure (Cro::HTTP2 suite, 7 subtests across 3 files)

`t/http2-request-serializer.rakutest` (and `http2-response-serializer`,
`http2-request-parser`): the test helper `sub test(..., *@checks)` taps
`Cro::HTTP2::RequestSerializer.transformer($fake-in.Supply)`. The mainline
builds check closures like `(*.headers eq $encoder.encode-headers(@headers))`
over a mainline `my $encoder = HTTP::HPACK::Encoder.new`. The serializer's
`whenever` body (`RequestSerializer.rakumod:13`) declares its own
`my $encoder = HTTP::HPACK::Encoder.new` on the supply worker thread. The
check closure is invoked from the tap callback **on that worker's call
chain**, so its `$encoder` read resolves to the SERIALIZer's encoder
(don't-overwrite merge finds "encoder" already in the caller env chain).
An HPACK encoder is stateful: the serializer already encoded the headers
once, so the test's re-encode emits dynamic-table references
(`190,130,135,191,192` instead of the 38-byte literal encoding) and
`check 4` fails. The `data eq $random` checks pass because the whenever
body has no `my $random` — only the `$encoder`-reading check is hijacked.
(The old ticket `todo/tickets/http2-data-frame-content-mismatch-...md`
misattributed the failure to the DATA frame's `.data`; the failing check
is the HEADERS frame's `.headers eq` — same helper line, "check 4" of
frame 1.)

Verified instrumentation (2026-08-11, temporary `MUTSU_DEBUG_ENC` prints in
the merge loop):

```
[ENC] merge id=800 captured=inst#795 is_cell=false existing_in_caller=inst#821
[ENC] post-merge id=800 env_encoder=inst#821 writes=false cwrites=false auth=false
```

Captured env holds the CORRECT mainline instance (#795); the caller env
already holds the serializer's whenever-local (#821); don't-overwrite keeps
#821; the closure body then reads #821. Renaming the serializer's inner
`my $encoder` to `$enc-renamed` (shadow-lib experiment) makes the test
pass — pure name collision.

## Repro

`tmp/h2-bisect14.raku` (self-contained against the Cro checkout under
`tmp/cro-work/`, run with the `inc-paths.txt` `-I` list). Notable
sensitivity: the hijack only manifests when `use Cro::HTTP2::RequestParser;`
is also present in the test file (with it: caller env sees the whenever
frame's #821; without it: caller env sees the mainline value — the supply
drive/thread structure differs). This use-sensitivity is a SECONDARY
trigger — the merge-policy hole is the primary defect — but it explains
why earlier synthetic repros (`tmp/h2-min1.raku`, `tmp/h2-min2.raku`,
mirroring the supply/whenever/tap shape without the extra `use`) failed
to reproduce. Do not attempt to re-shrink below the `use` set.

Also reproduced in-file: mainline `my $host; $host = "MAIN-HOST"` is NOT
hijacked even though the whenever body declares `my $host;` — an
uninitialized `my` writes no env entry (slot only), so the caller chain has
no "host" key and or_insert installs the capture. Only a
declared-AND-initialized inner `my` (`my $encoder = HTTP::HPACK::Encoder.new`)
lands in the worker's env and hijacks.

## Why this is deep, not a ticket

The sound fix direction (per CLAUDE.md gain/risk: prefer cell mechanisms
that cannot go flaky over heuristic gates) is to extend eager cell boxing
so that a closure capture of a creator-written scalar ALWAYS goes through a
shared `ContainerRef` cell — cells both win the merge (overwrite branch)
and keep post-capture creator mutations visible, satisfying the two
requirements that currently conflict. That is effectively the next slice of
ADR-0024 (mainline lexical cells), generalized from "mainline named subs'
free variables" to "any closure capturing a mainline/creator-written
scalar" — including plain anon/pointy closures passed across threads.
Alternatives (widening `authoritative_free_vars` to "never written AFTER
the closure's creation point", or making the merge overwrite for
`cc.free_var_syms` members) each break a documented behavior
(`my $s = 0; @cb.push({ $s }); $s = 42` must read 42; see the comment at
the merge site). Needs an ADR-level decision on boxing scope + perf
measurement (boxing cost was the reason lever C stayed narrow, #2749).

## Second manifestation: http-session expiration (the OPPOSITE direction)

`t/http-session-inmemory.rakutest` / `t/http-session-persistent.rakutest`
"Session expires appropriately" (expected 'Visit 1', got 'Visit 4'): the
test passes `now => { $fake-now }` into
`Cro::HTTP::Session::InMemory[...]`, then advances the mainline
`$fake-now += Duration.new(...)` between requests. The closure is stored in
the middleware's `&.now` attribute and called on the server's worker
threads; under mutsu it returns the CREATION-time value forever (verified
by shadow-lib instrumentation of `SessionStore!delete-expired`: `&!now()`
stayed at the initial Instant across all requests while head-exp never
moved, so sessions never expire). This is the mirror image of the
`$encoder` hijack: there the capture must win over an unrelated caller
binding; here the creator's POST-capture mutations must stay visible to
the (cross-thread) closure. A trivial Channel-based synthetic
(`tmp/cross-thread-live-read.raku`) works — the name-keyed lane sync
covers it — but the real path (closure stored in an attribute, invoked on
a worker several spawn generations deep) loses the update. A shared-cell
capture satisfies both directions at once, which is why the cell route is
the fix direction rather than merge-order tweaks.

Related same-family ticket (a THIRD direction — captured value wins over
the closure's own inner for-loop parameter):
`todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`.
Any fix here must be validated against its 11-line repro too.

## Verification (once fixed)

- `tmp/h2-bisect14.raku`: `in-check enc WHICH` must equal `mainline WHICH`.
- `bash tmp/cro-suite-run.sh http`: `http2-request-serializer.rakutest`,
  `http2-response-serializer.rakutest`, `http2-request-parser.rakutest`
  all `notok=0`; `http-session-inmemory.rakutest` /
  `http-session-persistent.rakutest` "Session expires appropriately" pass.
- The merge-site comment's regression example must keep passing:
  `my $s = 0; my @cb; for 1..3 { @cb.push({ $s }) }; $s = 42; say @cb[0]()`
  → 42.
- roast: S12-construction/roles-6e.t (the by-value-capture flake family),
  S17-lowlevel/cas.t (cas resolves by name; cells broke it before —
  see the `type_constrained_unboxable` skip in `box_captured_lexicals`).
