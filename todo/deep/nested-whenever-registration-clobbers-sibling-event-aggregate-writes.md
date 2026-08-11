# Nested `whenever` registration makes a later sibling event's aggregate write clobber the supply block's hash

Root cause of the sole remaining failure in the vendored Cro::HTTP
`http2-request-parser.rakutest` (test 49, "check 4" of
'Header1 + Header2 + Data1 + Data2') — and the reason HTTP/2 stream demux
loses DATA frames.

## Minimal repro (dependency-free, deterministic, single-threaded)

`tmp/streams-hash-clobber.raku` (also inlined here). Note there is NO `start`
block — everything runs on the main thread through the emit-driven tap path:

```raku
my $trigger = Supplier.new;
my $done = Promise.new;
my $s = supply {
    my %streams;
    whenever $trigger.Supply -> $sid {
        unless %streams{$sid}:exists {
            %streams{$sid} = "S$sid";
            my $cancellation = Promise.new;
            whenever $cancellation { note "cancelled" }   # <-- the poison
        }
        note "after write $sid: keys={%streams.keys.sort.join(',')}";
        emit $sid;
    }
};
$s.tap: -> $v { $done.keep if $v == 5 };
$trigger.emit(3);
$trigger.emit(5);
await Promise.anyof($done, Promise.in(3));
```

raku: `after write 5: keys=3,5`. mutsu: `after write 5: keys=5` — the
event-3 entry is gone.

## Bisect matrix (all in `tmp/streams-hash-clobber*.raku`, raku-validated)

| variant | shape | mutsu result |
|---|---|---|
| A (`...2`) | no nested `whenever` at all | correct (`3,5`) |
| B (`...3`) | nested `whenever` every event, write BEFORE registration | broken (`5`) |
| C (`...4`) | nested `whenever` only on event 3, write BEFORE registration | broken, and the smoking gun: event 5 **reads** `keys=3` at its start, then after its own `%streams{5} = ...` **reads back `keys=5`** |
| D (`...5`) | nested `whenever` only on event 3, registration BEFORE the write | correct (`3,5`) |

Variant C proves the corruption point precisely: within one callback
invocation, consecutive statements see `{3}` (read) then `{5}` (after
writing key 5). The indexed-assignment write resolved `%streams` to a
DIFFERENT, stale container (one that predates event 3's write — i.e. the
empty creation-time state), wrote into that fork, and installed it, after
which reads follow the fork. At event end the fork is published, clobbering
the live `{3}` container that the enclosing supply body (and the Cro DATA
handler) uses.

Variant D shows the fork's base tracks writes made BEFORE the registration
in the same event — registering first and writing after stays consistent.
So the stale base is captured/forked by the nested-whenever registration
machinery, not by the write itself.

## Where to look

- `run_whenever_with_value` (`src/runtime/subtest.rs`): whenever callbacks
  are built with `self.env.clone()` and `owned_lexicals` marked
  authoritative (overwrite-install at dispatch). A nested `whenever`
  registered mid-event goes through the
  `!self.supply_emit_buffer.is_empty() || self.react_active > 0` branch or
  the dispatch-time group-join path below it.
- The emit-driven tap dispatch (`native_supply_mut_methods.rs`,
  `call_supply_tap` family) — how the outer callback's env is installed per
  event, and what changes about that installation once a nested marker has
  been adopted (variant A vs B/C differ only in that adoption).
- The aggregate name lane: `%`-sigil containers are reference-shared
  (writes normally hit the one shared `HashData` in place via
  `arc_contents_mut`), so the observed divergence means the name got
  REBOUND to a forked container somewhere in the adoption path, not that a
  COW write missed.

## Cro mapping

`Cro::HTTP2::GeneralParser.transformer` registers `whenever $cancellation`
inside the `when Cro::HTTP2::Frame::Headers` branch, after writing
`%streams{$curr-sid} = Stream.new(...)` — exactly variant C. Instrumented
run (shadow copy of GeneralParser) shows: HEADERS(5) reads
`known-before=3`, writes, and the hash becomes `{5}`; the subsequent
DATA(3) frame finds no stream and is silently dropped (`if $stream` guard),
so request 0's body promise never resolves; stream 5's Stream object in the
forked hash is disconnected from the one whose body supply the emitted
request actually holds, so its 246-byte DATA emit is lost too and the body
resolves empty (`body-len=0`; raku: 123/246). Probe:
`tmp/h2rp-probe.raku` + instrumented shadow recipe in
`tmp/shadow/lib/Cro/HTTP2/GeneralParser.rakumod` (rebuild from the vendored
original + the DBG notes if the shadow was cleaned).

Note: `@`/`%` aggregates are explicitly OUT of ADR-0025 slice 1/2 scope
(the cell campaign covers plain `$` scalars), so this is not expected to be
fixed by slice 2 — it is its own dispatch/env-provenance bug in the
supply/whenever machinery, closest in spirit to ADR-0023 binding
provenance and the ADR-0010 lane rules.

## Acceptance

- The four repro variants match raku.
- `http2-request-parser.rakutest` passes fully (with the TAP-counter fix
  from PR #6238 already in, test 49 is the only remaining failure).
- Watch `http2-response-parser.rakutest` too — same GeneralParser code path
  drives it.
