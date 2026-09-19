# Package-chain lookup drops `StrSearcher` overhead

Line-level `callgrind` profiling of a `JSON::Fast.from-json` parse
([#8673](https://github.com/tokuhirom/mutsu/issues/8673)) found
`<core::str::pattern::StrSearcher>::new` and its `TwoWaySearcher`/
`CharSearcher` machinery costing roughly 15-20% of total instructions
retired, almost all of it reachable from a single call site:
`Registry::lookup_in_package_chain`'s `pkg.rsplit_once("::")`, reached
through `unit_lexical_slot` — the resolver every free-variable read that
misses its own lexical bucket takes, walking up to four candidate packages
per read. Parsing just 100 flat JSON records called this path roughly
226,000 times, with its internal `rsplit_once("::")` loop combining for
about 1.3 million calls into the `StrSearcher` constructor path.

The earlier function-level profile of this same workload (#8686) could not
see this: it ran against a plain `cargo build --release` binary, and this
repo's `[profile.release]` strips debuginfo by default, so `callgrind`
could only attribute cost to whole functions. Rebuilding with `cargo build
--profile profiling` (release optimizations plus debuginfo, already
provided in `Cargo.toml`) gave real source-line attribution and surfaced
this specific call site.

`str::rsplit_once(&str)` goes through the generic `Pattern` machinery and
builds a full Two-Way search state (critical factorization, period
computation, ...) even for a fixed 2-byte ASCII needle. This is the same
overhead `src/runtime/utils/str_scan.rs` already documents and fixes for
`.contains("::")` and `.split_once('[')` (`has_double_colon`,
`split_once_bracket`, from #7554/#7696) — `rsplit_once("::")` was the one
remaining hot spelling not yet covered.

Added `rsplit_once_double_colon`, a manual reverse byte scan matching
`str::rsplit_once("::")`'s semantics exactly (tested for agreement,
including overlapping `:::`/`::::` runs and non-ASCII payloads around the
separator), and applied it at the four call sites the profile implicates:
`lookup_in_package_chain`, `lookup_in_package_chain_mut`, and
`unit_lexical_slot`/`unit_lexical_slot_mut`'s qualified-name split.

Measured (clean A/B release builds, 5 runs each, this box) on the
synthetic SPDX-shaped reproduction from #8673: ~8.7% faster at 100 records
(0.463s -> 0.423s) and ~9.0% faster at 727 records (3.690s -> 3.358s).
`unit_lexical_slot`'s package-chain walk is a general free-variable
resolution path taken by any module whose lexicals fall to the general
binder, not something specific to `JSON::Fast`, so the improvement should
carry over broadly rather than being confined to this one reproduction.

This closes #8673's own narrower scope ("this one parse should not time
out" — already true since #8686's Phase 0/Phase 2 landed, and now a further
~9% faster on top). Closing the remaining gap to `raku` on this workload is
further profiling-heavy work, better tracked as its own fresh `todo:perf`
issue than kept open here.
