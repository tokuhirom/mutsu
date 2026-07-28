# Regex subrules match the whole subject in place instead of a re-slice

`docs/adr/0016-span-based-captures-and-lazy-match.md` phase 1.

Until now a subrule call (`<foo>`) matched its body against `&chars[pos..]` with
`start = 0`. Every offset the body produced was therefore *slice-relative*, and the
caller rebased the whole result afterwards with `shift_capture_descendants`, which
recurses through the capture tree calling `Arc::make_mut` on each nested subcapture.
Those `Arc`s are never unshared — `record_reduced_subrule` clones each one into the
`REDUCED_SUBRULES` thread-local the moment it is built — so `make_mut` took the
copy-on-write path every time: **each subrule call deep-copied its entire descendant
capture subtree, at every level of nesting, for every candidate tried, including the
ones that went on to lose.** A depth-`d` parse copied its capture tree `d` times.

Subrules now match against the whole `chars` starting at `pos`. Offsets are absolute
from birth, so nothing is rebased: `shift_capture_tree` / `shift_capture_descendants`
are gone, and with them the deep copy. `REGEX_PRECEDING_CHAR` — a thread-local plus
RAII guard whose only job was to tell a slice which character preceded it — is gone
too, along with the `pos + inner_end` / `pos + capture_start` arithmetic at the two
subrule call sites. Net −104 lines.

## Four constructs were wrong, and now are not

The re-slice did not just cost copies; it hid the text before the subrule from anything
that looks backwards. Verified against `raku`, all four previously disagreed with it:

| grammar | raku | mutsu before | mutsu now |
|---|---|---|---|
| `token TOP { 'ab' <t> }` / `token t { << \w+ }` on `abcd` | no match | **match** | no match |
| `token t { <?after 'ab'> \w+ }` on `abcd` | match | **no match** | match |
| `token t { <!after 'ab'> \w+ }` on `abcd` | no match | **match** | no match |
| `token t { <at(2)> \w+ }` on `abcd` | match | **no match** | match |

`<<` / `>>` / `<?wb>` read `chars[pos - 1]`, which at slice-position 0 is "nothing", so
a word boundary fired in the middle of a word; look-behind could not see behind the
subrule's start at all; `<at(N)>` meant "position N *in the slice*". `^^` was the one
case that already worked, and only because of the `REGEX_PRECEDING_CHAR` workaround.
Pinned by `t/regex-subrule-absolute-position.t` (9 subtests, green under `raku` too).

`<{ ... }>` closure interpolation and the cursor-method path (`regex_token_method.rs`)
now see the full subject as well, which is what `.orig` and a cursor's coordinates are
supposed to be.

## Why this shape

A clean profile of `benchmarks/bench-yaml-parse.raku` after the previous three rounds
has no dominant function left: ≈28% allocator (`malloc`/`_int_malloc`/`_int_free`/
`cfree`/`realloc`/`malloc_consolidate`), 11% `memcmp`, 5% `memmove`. That is a data
model that allocates and copies per match step, not a hot call site — so ADR-0016
commits to the representation NQP/MoarVM uses (spans into one shared subject, a lazily
materialized `Match`) and phases it. This is P1, the phase the rest depends on: a span
means nothing until positions are absolute.

Measure from `bench-history.tsv` on the `bench-data` branch, not a local A/B — see the
ADR and `todo/tickets/yaml-parse-throughput.md` for why local numbers on the dev box
were not trustworthy in the previous rounds.
