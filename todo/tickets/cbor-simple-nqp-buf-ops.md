# CBOR::Simple needs the nqp buffer op family — gates Log::Timeline, and therefore Cro::HTTP

Found while probing `use Cro::HTTP::Router` after the brace-subscript parser
fix. The load chain is:

```
Cro::HTTP::Router → Cro::HTTP::Response → ... → Cro::HTTP::LogTimelineSchema
  → Log::Timeline            (Log/Timeline.rakumod line 2 eagerly does)
  → Log::Timeline::Output::CBORSequence
  → CBOR::Simple             ← dies: "Unsupported nqp:: op: nqp::bitor_i" (line 124)
```

So the web-framework survey's "no `use nqp` anywhere in the Cro chain" claim
holds for the 7 dists' own code but misses this **transitive** dep:
Log::Timeline (a hard dep of Cro::HTTP) eagerly loads its CBOR output backend,
and `CBOR::Simple` is a classic `nqp_ops_only` dist. Its op histogram
(`grep -o 'nqp::[a-z_0-9]*' lib/CBOR/Simple.rakumod | sort | uniq -c`):

- hot: `istype` (45), `add_i` (41), `writeuint` (40), `readuint` (24),
  `const` (24), `islt_i` (19), `while` (18), `decont` (17)
- bit ops: `bitshiftl_i` (13), `bitor_i` (11), `bitand_i` (4), `bitshiftr_i` (1)
- buf ops: `bindpos_i`/`bindpos_n`/`atpos_i`/`atpos_n`, `setelems`, `slice`,
  `splice`, `writenum`/`readnum`, `writeint`/`readint`
- misc: `iseq_i`/`isle_i`/`iseq_n`/`isne_n`, `isnanorinf`, `p6box_s`,
  `decode`, `elems`, `stmts`/`if` (control flow)

mutsu already dispatches a supported nqp subset
(`src/runtime/builtins_operators_fallback.rs` — the error text is the
fallthrough), so this is an *extension* of an existing mechanism, not a new
layer. The integer/bit/comparison ops are trivial; the work is the
`readuint`/`writeuint`(+int/num) family with `nqp::const` endian/size flags
over Buf/Blob, plus `nqp::while`/`nqp::stmts` control-flow forms if they are
not already handled.

Until this lands, `use Cro::HTTP::<anything that touches Request/Response>`
cannot even load under mutsu regardless of other fixes. (Workaround options
considered and rejected: patching the vendored Log::Timeline violates the
never-edit-vendored rule; a fake CBOR::Simple shim violates rung-3 policy.)

Repro (deps fetched from fez/REA as in the survey):

```
target/debug/mutsu -I <log-timeline>/lib -I <cbor-simple>/lib -e 'use Log::Timeline; say "ok"'
```
