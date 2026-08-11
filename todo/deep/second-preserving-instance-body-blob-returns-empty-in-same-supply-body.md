# The second of two `Supplier::Preserving`-backed `body-blob` reads in one HTTP/2 stream demux returns empty, even though `emit`/`done` fire on the right object

Found 2026-08-11 while verifying the fix for
`todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`
(now resolved — see `news/2026-08/` — via
`assign_hash_elem_to_shared_var`/`assign_array_elem_to_shared_var` bailing out
when the target is already a `ContainerRef` cell). With that fix in place, the
vendored Cro::HTTP2 `http2-request-parser.rakutest`'s `%streams` hash is now
byte-for-byte correct (matches raku: both stream 3 and stream 5 are present,
`DATA` frames are routed to the right `Stream` object), yet the file still has
exactly one failure — "check 4" of the "Header1 + Header2 + Data1 + Data2"
subtest — because a **second, independent** bug surfaces once the first is out
of the way: the *second* concurrently-open stream's `.body-blob.result`
resolves to an empty `Buf`, even though the underlying `Supplier::Preserving`
was `.emit`-ed and `.done`-ed on the exact same object identity the reader
holds.

## Repro (needs the vendored Cro checkout — no minimal Cro-independent repro found yet, see below)

`tmp/h2rp-probe.raku` run against the shadow-instrumented copy of
`Cro::HTTP2::GeneralParser` at `tmp/shadow/lib/Cro/HTTP2/GeneralParser.rakumod`
(debug notes already inlined there from a prior session — diff it against the
vendored original at
`tmp/cro-work/C_RO_CRO_HTTP_*/lib/Cro/HTTP2/GeneralParser.rakumod` to see
exactly what they print):

```
bash -c '
ROOT=/home/tokuhirom/work/mutsu-roast
BIN=$ROOT/target/debug/mutsu
INC=$(cat $ROOT/tmp/cro-work/inc-paths.txt)
DIST=$(echo $ROOT/tmp/cro-work/C_RO_CRO_HTTP_*)
timeout 30 $BIN -I $ROOT/tmp/shadow/lib $INC -I "$DIST/lib" tmp/h2rp-probe.raku
'
```

mutsu (with the `%streams` fix applied):

```
DBG DATA sid=5 known-streams=3,5 stream-found=True data-len=246 end=True
DBG DATA stream-obj=Stream|1013 body-obj=Supplier::Preserving|994
DBG DATA emitted sid=5
DBG DATA done sid=5
req 1 stream=5 body-len=0 expect-single=123 match-single=False match-double=False
```

raku (reference, same instrumented file):

```
DBG DATA sid=5 known-streams=3,5 stream-found=True data-len=246 end=True
DBG DATA stream-obj=Stream|4274809208920 body-obj=Supplier::Preserving|4274703709960
DBG DATA emitted sid=5
DBG DATA done sid=5
req 1 stream=5 body-len=246 expect-single=123 match-single=False match-double=True
```

Everything up to and including `DBG DATA done sid=5` is now identical to raku
— confirmed by `stream-obj`/`body-obj` `.WHICH` matching between HEADERS time
and DATA time in both runs — so this is not a repeat of the `%streams`
clobber. Stream 3's own `body-blob.result` (`req 0`) is correct (123 bytes);
only stream 5 (the *second* stream opened in the same `supply` body) loses its
body.

**Important: this is NOT new.** Re-running the identical probe against the
pre-fix binary (`git stash` the `%streams` fix, rebuild) shows the exact same
`req 1 stream=5 body-len=0` outcome — it was simply masked before because
`%streams{5}` itself was missing (`stream-found=False` for one of the two
streams), which produced the same *symptom* (empty body) via a different
route. This bug was always there; the other one just fired first.

## What has been ruled out

- **Not the `%streams` hash itself** — confirmed correct in both runs at every
  checkpoint (`known-streams=3,5`, `stream-found=True`, same `Stream`/
  `Supplier::Preserving` `.WHICH` identity throughout).
- **Not `emit`/`done` on the wrong object** — `DBG DATA emitted sid=5` /
  `done sid=5` fire, and `stream-obj`/`body-obj` match the ones captured at
  HEADERS time.
- **Not reproducible with a Cro-independent synthetic repro yet.** Two
  attempts (`tmp/preserving-second-instance2.raku`,
  `tmp/preserving-body-blob-repro.raku` — both create two
  `Supplier::Preserving`-backed objects inside a repeatedly-dispatched
  `whenever` body, keyed by a shared `%`-hash, each read back via a
  `Promise(supply { whenever $obj.Supply -> $v { ...; LAST emit $v } })`
  reduction pattern matching `Cro::MessageWithBody.body-blob`) both **fail to
  reproduce** — both streams resolve correctly in mutsu. So the real trigger
  needs something the minimal repros are missing; candidates not yet tried:
  - The reader (`$request.body-blob.result`) running in a genuinely different
    **thread/frame** than the writer, specifically the *test harness's own*
    `.tap: -> $request { start { ... } }` (a second, independently-spawned
    `start` per emitted request) rather than a `start` spawned from inside the
    `supply` body itself.
  - `Cro::HTTP::Request`/`Cro::MessageWithBody`'s actual attribute-cell
    mechanics (`has Supply $!body-byte-stream`, `method
    body-byte-stream(--> Supply) { with $!body-byte-stream { $_ } ... }`) —
    the minimal repro used a plain `has Supplier::Preserving $.body` instead
    of the real role's private-attribute-with-a-`with`-block indirection.
  - The outer `supply { }` block's `emit-response`/`emit $message` timing:
    the real code emits the *response* (with body still streaming in) at
    HEADERS time, before the DATA frame for that stream arrives — so the
    consumer's `.body-blob.result` genuinely starts waiting on a `Supply`
    that has not emitted yet, and only fills in later, cross-thread. The
    minimal repros above emit the request only when data is ready or in a
    different order.

## Suggested next steps

1. Narrow the Cro-independent repro further, closest first: emit the message
   to the tap (and start its `body-blob.result` read) *before* the data
   arrives, exactly like `GeneralParser.transformer` does at line 136-137.
2. If a minimal repro forms, `rust-gdb` breakpoints on the `Supplier::
   Preserving` tap/backlog machinery (`src/runtime/native_supply_mut_methods.rs`,
   the preserving-backlog code that was already fixed once for an
   *immutable*-lane asymmetry in `supplier-preserving-backlog-destroyed-by-
   done-immutable-lane.md`, PR #6166) to see whether the SECOND
   `Supplier::Preserving` instance created in one process ever gets a working
   backlog/tap registration, or whether some global/singleton state meant to
   be per-instance is shared across the two.
3. Cro mapping / acceptance: `http2-request-parser.rakutest` "check 4" of
   "Header1 + Header2 + Data1 + Data2" (currently the sole remaining failure)
   and any other Cro suite file exercising two-or-more concurrently open
   HTTP/2 streams with bodies.
