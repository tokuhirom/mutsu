# A literal-prefix prefilter for unanchored regex scans

ADR-0099 §2.4 measured mutsu asking itself the same question two ways on one
640 KB subject: `.index('needle')` in 0.4 ms, `~~ /'needle'/` in 84.7 ms —
**212x**, because mutsu already ships a substring search 10x faster than
rakudo's and the regex engine simply never used it. Every unanchored scan
entered the full backtracking engine at every character position, ~983
instructions to establish that `chars[i] != 'z'`.

This wires that existing primitive into the scan loops (`regex_match_find.rs`,
`regex_match_public.rs`, `regex_eval_class.rs`) for the case ADR-0099 §4 calls
"not a new optimization to invent — wiring an existing primitive": a pattern
whose body (or a leading run of it) is a plain, unconditional, non-`:i`/`:m`
literal. `required_literal_prefix` derives that once per parsed pattern as a
conservative *subset* of `regex_ltm_rank.rs`'s existing declarative-prefix
construction table (`ltm_litlen_walk`) — same chain-ending rules (a
quantifier, a separator, a capture alias, non-constant interpolation) — so
the two definitions cannot drift, per the ADR's own non-negotiable
constraint. Everything the ADR's "what to build" section lists beyond this —
alternation-derived first-character sets, a required *inner* literal, `:i`
fold-closure first-sets, `:m` NFD-aware first-sets, subrule-derived prefixes
— is out of scope for this slice and simply declines, which is always safe:
it only costs the prefilter's benefit, never correctness.

Measured on the ADR's own headline scenario (640 KB subject, failing literal
scan, release build, this box): ~79.5 ms → ~8.6 ms.

A new `MUTSU_REGEX_PREFILTER=off` kill switch and a differential property
test (`tests/regex_prefilter_differential.rs`) assert the same corpus
produces byte-identical output with the prefilter forced on vs off — the
gate ADR-0099 §7 calls for, since this is the first piece of regex machinery
that can be *wrong without being incorrect* (an over-promising analysis could
silently drop a valid match). New `MUTSU_VM_STATS` counters
(`regex-prefilter: applied/declined/positions_offered/position_hits`).

[#8272](https://github.com/tokuhirom/mutsu/issues/8272) stays open — the
first-character-set, required-inner-literal, fold-closure, and
subrule-prefix work this slice deliberately deferred is still tracked there.
