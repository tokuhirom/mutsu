# Parsing YAML with the bundled `YAMLish` is still ~5-35x slower than raku

The YAML battery is correct — all 5 upstream files (81/81 subtests) pass — but it
is **slow**, and the cost is in *matching*, not module load. This is the
match-time twin of `grammar-heavy-module-load-slower-than-raku.md`; that ticket
measures `use`, this one measures the parse itself.

**Two rounds of this have already landed.**

Round 1 (`news/2026-07/regex-code-block-writeback-by-identity.md`):
`eval_regex_code_block_body` used to snapshot the whole env with
`format!("{:?}", v)` before and after **every** regex `{ … }` block and compare
the strings. `core::fmt` was ~20% of a `load-yaml` profile before that; comparing
by `Value::same_binding()` instead made a block mapping **4.7x faster**.

Round 2 (`news/2026-07/grammar-actions-skip-dispatch-for-missing-methods.md`):
`invoke_grammar_actions` called `call_method_with_values` for **every**
match-tree node regardless of whether the actions class defined a method for
that rule, relying on `MethodNotFound` as the "no action" signal. Most nodes
are low-level helper tokens with no action method, so this paid full
multi-dispatch resolution (`has_proto`/`has_multi_candidates`/
`has_multi_function`/`resolve_function_with_types`/`bare_name_packages` plus a
`format!`-built error) just to fail. A cheap `has_user_method` pre-check (a
direct MRO + HashMap-by-name lookup, no candidate scan) now skips the call
entirely when neither the `:sym<...>` variant nor the plain rule name is
declared — this required handling `ValueView::Package` (a stateless
`:actions(Actions)` grammar action is commonly the bare type object, not an
instance) alongside `ValueView::Instance`, or the class name never resolves
and the pre-check is a no-op. A `perf` profile of a space-heavy block-sequence
document showed the dispatch cluster (and its `malloc`/`format!` downstream)
shrink to a small residual, with total profiled samples for the same input
roughly halving.

What is below is what remains *after both* fixes.

## Measurement (2026-07-28, release build, pre-round-2)

The table below predates round 2 (it was taken right after round 1 landed) and
was re-derived this session as: `basic.rakutest`'s slowest subtest is its
second document (`message`/`dump:`/`comment:`), and bisecting *that* down
further showed the cost is proportional to the length of the single **run** of
consecutive spaces inside a quoted scalar (`'      16G         05C        '`),
not to item count or overall document size — e.g. a lone `'a' ~ (' ' x
$n) ~ 'b'` list item cost roughly a flat ~12ms/space in release regardless of
$n. Round 2's fix removes a large piece of that flat per-position cost (each
space position walks the quoted-scalar grammar's `space`/`single-bare`/
`single-quotes`/`foldable-whitespace` alternatives, and *every one* of those
subrule matches used to pay the full action-dispatch resolution this ticket's
round 2 removed) — but this table itself was not re-measured locally, since a
reliable A/B needs an idle box or the `bench-data` CI history, not a
contended local run (see "Delegate the full roast run to CI" in `CLAUDE.md`).

Synthetic `k$_: v$_` block mapping under `load-yaml`:

| lines | mutsu (before) | mutsu (now) | raku |
| --- | --- | --- | --- |
| 16 | 1127ms | **568ms** | 196ms |
| 64 | 10065ms | **2147ms** | 442ms |

The super-linearity is largely gone (4x the input is now ~3.8x the time), and the
ratio to raku fell from 23x to ~5x.

Whole upstream test files, however, did **not** all move:

| File | before | now | raku |
| --- | --- | --- | --- |
| `anchor-alias.rakutest` | 1.4s | 0.6s | — |
| `p5-tests.rakutest` | 1.7s | 0.5s | — |
| `roundtrip.rakutest` | 10.0s | 5.9s | 1.4s |
| `test-harness.rakutest` | 24.1s | 18.6s | 1.9s |
| `basic.rakutest` | 45.5s | **43.6s** | 1.2s |

`basic.rakutest` barely improved, so **its documents hit a different dominant
cost** — that is the next thing to find. Its inputs are the largest and the most
feature-dense (nested block sequences inside mappings, explicit `? key` /
`: value` pairs, flow collections, folded scalars, `%TAG` directives).

## Reproduce

```sh
cargo build --release
cat > tmp/y.raku <<'EOF'
use YAMLish;
my $n = (@*ARGS[0] // 16).Int;
my $text = "---\n" ~ (1..$n).map({ "k$_: v$_\n" }).join;
my $t0 = now;
load-yaml($text);
say "n=$n elapsed=", ((now - $t0) * 1000).round, "ms";
EOF
for n in 16 64; do ./target/release/mutsu tmp/y.raku $n; done
# and the file that did not improve (fetch the suite at the pinned commit first):
time ./target/release/mutsu <yamlish-checkout>/t/basic.rakutest
```

Profile with a `--profile profiling` build (release + debuginfo) under `perf`.
The `MUTSU_VM_STATS` counters are useless here: only ~25k opcodes run for a
16-line document, so essentially all of the time is native regex-engine code.

## Where to look next

1. **Re-profile `basic.rakutest` after round 2, on an idle box (or via CI).**
   This session's box had 2-3 concurrent `cargo build --release` jobs from
   other sessions running throughout (system load 13-15), which makes any
   local wall-clock number here unreliable — use `perf`'s *relative* sample
   distribution (not absolute time) for structural comparisons, and the
   `bench-data` branch history for the number that actually goes in a
   document, per "Benchmark numbers in documents come from the bench CI" in
   `CLAUDE.md`.
2. **Per-call token instantiation turned out to be a dead end — already
   memoized for the case that matters.** The investigation this session
   confirmed `resolve_token_patterns_with_args_in_pkg`'s re-parse-per-call cost
   (described in the original version of this item) is real but *rare*: a
   debug-build call count on a 30-space synthetic document showed only ~19
   with-args calls total (`block`/`sequence`/`map`/`map-entry`/`list-entry`/
   `element`/... once or twice each), independent of the number of spaces.
   The *argument-less* subrules that actually scale with input size
   (`space`, `single-bare`, `single-quotes`, `foldable-whitespace` — one
   alternative-set try per character in a run of spaces) already go through
   `PARSED_TOKEN_CANDIDATES`, a per-(pkg,name) memo added in #4587 — so their
   *resolution* was never the per-character cost. What scaled with input size
   was the *action-dispatch* attempt after each of those matches succeeded,
   which is round 2's fix. If a future profile still shows
   `resolve_token_patterns_with_args_in_pkg` (not `resolve_parsed_token_candidates_in_pkg`)
   as hot, THEN revisit the "(rule, pkg, args)" memo idea above — but verify
   with a call-count instrumentation first, the way this session did, rather
   than assuming from the function's doc comment.
3. **Candidate enumeration.** The `_all_` atom enumerations return every end
   position; with a deeply nested indentation grammar the branching multiplies.
   Not yet measured directly — still open.
4. **Residual `malloc`/`_int_free`/`__memcmp_avx2_movbe` cost.** After round 2,
   these plain allocator/libc symbols are the largest entries in a `perf`
   self-time sort of the space-heavy synthetic (no single mutsu function
   dominates). This is consistent with genuine O(n) `Match`/capture object
   construction — one per matched space character, since each is its own
   quantified `<str=space>` capture — rather than a single fixable call site.
   Reducing it would mean cutting per-attempt allocation in the regex engine's
   capture-construction path itself (e.g. `make_subcap_match`,
   `RegexCaptures` cloning in the `Alternation` match-merge loop in
   `regex_match_atom.rs`), which is a different, deeper investigation than
   either round so far.

## Why it matters

A bundled battery is loaded and *used* on every run of a program that needs it.
Reading a 100-line config file must not take seconds.
