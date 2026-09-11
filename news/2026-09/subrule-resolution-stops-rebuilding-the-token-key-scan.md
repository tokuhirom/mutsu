# Subrule resolution stops rebuilding the token-key scan on every reference

Round 11 of the YAML-throughput investigation ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
Round 10 ended with "no single dominant site any more — allocator traffic ~20%,
`memcpy` 6.7%, `LocalKey::with` 5.7%, SipHash 8.1%". A fresh
`valgrind --tool=callgrind` profile of a 120-row YAML document said otherwise:
one call site owned **15% of the whole program**, and it was hiding inside
those flat allocator/TLS/hash totals rather than under its own name.

Measured on a 60-row document (`benchmarks/bench-yaml-parse.raku`'s shape with
`$ROWS = 60`, two sections), instructions retired go **8.13 Bn -> 5.89 Bn, a
27.5% cut**. Instruction counts are deterministic and load-independent, so they
— not wall clock on a shared container — are what the four changes below are
attributed against; the local wall clock moved with them (1.13s -> 0.95s at 60
rows, 2.49s -> 2.02s at 120), but per `CLAUDE.md` the number that belongs in a
document is the `bench-data` CI series, which this entry predates.

## (a) Five copies of the proto-variant scan allocated a `String` per registry key

Resolving any `<subrule>` collects the rule's `:sym<…>` proto candidates by
walking **every** key of `Registry::token_defs` and prefix-testing it. All five
hand-rolled copies of that walk spelled the test as
`.map(|key| key.resolve())` — and `Symbol::resolve` is `as_str().to_owned()`,
so each one built an owned `String` for every key just to run `strip_prefix`
against it and drop it again.

On the profiled document that was 50,634 scans x ~92 keys = **4.6M
allocations**, 4.6M `Symbol::as_str` thread-local round trips and 4.4M
`memcmp`s: 1.11 Bn instructions, 15.2% of the run, and 43% of every allocation
the program made. It never appeared in a self-cost profile under its own name
because the cost sat in `malloc`/`free`/`LocalKey::with`/`memcmp`.

The five copies are now one helper, `Interpreter::proto_variant_keys_sorted`,
which filters on the interned `&'static str` that `Symbol::as_str` already
hands out and returns the matching keys **as symbols** — so no call site
re-interns a key it just read out of the map either. `-11.6%`.

## (b) …and then re-derived the same few lists thousands of times

With the allocations gone the scan was still O(all registered token keys) per
`<subrule>` reference, for a handful of distinct prefixes: 8.5% of the run.
It is now memoized per prefix behind the `TOKEN_DEFS_GEN` generation, the same
invalidation discipline `PARSED_TOKEN_CANDIDATES` and `REGEX_PARSE_CACHE`
already use. The regex `:my`-declarator path restores a whole `token_defs`
snapshot when the match ends and did **not** bump that generation on the way
out (only the declaration bumped it on the way in), so the restore now goes
through `Interpreter::restore_token_defs`, which does — closing a staleness
window that the pre-existing generation-keyed memos shared. `-8.5%`.

## (c) The regex engine's hot maps paid SipHash

`RegexCaptures`'s `named`/`regex_vars`/`capture_alias_map`/`hash_captures`, the
three left-recursion bookkeeping tables (`LR_MEMO`/`LR_ACTIVE`/`LR_SEED_READ`,
probed up to seven times per subrule activation) and the call-graph
streamability memos were all `std::collections::HashMap`, i.e. SipHash. Their
keys are interned `Symbol`s (a `u32`) and grammar rule names — not adversarial
input — and the maps are rebuilt several times per matched capture:
`sip::Hasher::write` plus `BuildHasher::hash_one` came to 7.9% of the run.
Switching them to `rustc_hash::FxHashMap` (already a dependency, already used
for the symbol table and the compiled-function maps) takes that to 1.7%.
`-7.1%`.

## (d) `<subrule>` atom text was re-parsed on every match attempt

`parse_named_regex_lookup_spec` splits an atom's text (`<name=.rule(args)>`)
into its silent/alias/token-lookup/argument parts. It is a pure function of
that string, and it ran once per *match attempt* — 232k calls, 813k
`trim_matches`, 232k allocations, ~2.6% of the run with its callees, for a
handful of distinct strings. It is memoized now, returning a shared
`Arc<NamedRegexLookupSpec>`. In the same vein `PARSED_TOKEN_CANDIDATES` is
keyed by `(Symbol, Symbol)` instead of `(String, String)`, so probing it no
longer allocates two owned strings per probe. `-3.6%` between them.

## Where this leaves the ticket

mutsu parses the 120-row document in ~2.0s against rakudo v2026.07's ~0.27s on
the same container: still ~7.5x, down from ~9.2x. The remaining profile is
genuinely flat — `malloc`/`free`/`_int_malloc` ~21%, `memcpy` 6.2%,
`LocalKey::with` 4.6% spread across a dozen callers, and no mutsu function
above 1.6%. The next round is the per-capture `Match`/`RegexCaptures`
construction traffic itself (the ticket's long-standing item 4), not another
call-site fix.

`t/grammar/grammar-qualified-proto-candidate-scan.t` pins the semantics the
rewritten scan has to keep — derived-adds-to-base, MRO dedup, the
declaration-order LTM tie-break and the `<Pkg::rule>` qualified form — with all
eight expectations verified against rakudo v2026.07.
