# TRIR gets typed forms for `nqp::substr` and `nqp::eqat`

[#8900](https://github.com/tokuhirom/mutsu/issues/8900) measured two costs left
over from ADR-0110 Stage 2: `nqp::` op bodies at 16.9% of a `JSON::Fast`
decode and the general binder at 16.1%, both because most of the namespace
still runs through TRIR's generic, boxed-dispatch op (`NqpOpGen` →
`dispatch_nqp_op_by_id`) even inside a routine that otherwise compiles to
typed, resolved IR. `nqp::ordat`, `nqp::atpos_i` and `nqp::chars` already had
typed forms; `nqp::substr` and `nqp::eqat` — the two ops `JSON::Fast`'s
`parse-string` fast path calls on every clean (all-word-character) string
token — did not.

This adds `SubstrS` and `EqAtS`: operand-direct typed ops that read straight
off the native/boxed banks, share the existing per-frame codepoint memo
(`TrCharCache`) that `ordat`/`atpos_i`/`chars` already use, and skip the
boxed-argument `Vec` build and id-indexed dispatch table entirely. Only the
3-argument shapes (`nqp::substr($s, $from, $len)`,
`nqp::eqat($haystack, $needle, $pos)`) get the typed form — the ones
`parse-string` actually calls; any other arity still declines to the generic
op, unchanged.

## Measurement

`tmp/jf-decode.raku` (ADR-0110 §8's own decode benchmark), 100 synthetic
license records, `--profile profiling` + callgrind, warm module-precompile
cache (second run quoted, per the perf-tuning skill's §0):

| | before | after |
| --- | ---: | ---: |
| total Ir | 3,606,094,911 | 3,605,119,203 |
| `run_trir_chunk` → `dispatch_nqp_op_by_id` calls | 9,214 | 3,004 |
| that edge's inclusive Ir | 35,480,000 (0.98%) | 30,631,730 (0.85%) |

The call count for TRIR's generic nqp-op dispatch dropped 67% (every
`substr`/`eqat` call in this document's *object keys*, which are plain
identifiers and always take the fast path), but the total-program effect is
small — about 0.13 percentage points, inside this benchmark's own noise
floor on the whole-program total. The reason is specific to this fixture's
shape, not a flaw in the approach: `parse-string`'s fast path requires the
*entire* string to be `CCLASS_WORD` (letters/digits/`_`) between the quotes,
and this benchmark's string *values* are license IDs with hyphens
(`LIC-42`), multi-word names (`Some License Name 42`) and URLs — none of
which qualify, so they all fall through to `parse-string-slow`, which this
change does not touch. Only the object *keys* (`licenseId`, `name`, ...) and
any all-word string values take the sped-up path here. A document whose
values are themselves plain words or identifiers — closer to `META6.json`'s
`depends`/`provides` lists, which is the workload `JSON::Fast` performance
was chased for in the first place — would see a larger share of its strings
on the typed path.

Also measured: `t/vm/codegen/adr0110-trir-differential.t` (extended with
`substr`/`eqat`/mixed shapes in `t/fixtures/trir-shapes.raku`) confirms TRIR
on and off agree exactly, and a new `trir::tests::substr_and_eqat_use_typed_ops`
unit test pins that the 3-argument shape actually compiles to `SubstrS`/
`EqAtS` rather than declining or falling to the generic op.

## What is still open

Profiling this same benchmark also showed `nqp::findnotcclass` — evaluated
unconditionally once per string token to test whether the fast path even
applies — as a comparably-sized generic-dispatch cost, with `is_cclass`'s
per-character scan underneath it as the larger piece. That scan cost would
survive a typed `findnotcclass` form; only the boxing/dispatch wrapper around
it would go. Left for a follow-up slice of #8900, which this PR does not
close — the issue's own text says correctly that no single slice here is
the fix for the wider gap.
