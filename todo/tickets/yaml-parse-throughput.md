# Parsing YAML with the bundled `YAMLish` is still ~5-35x slower than raku

**Update (2026-07-30, round 4): ~7x now.** The dominant cost was not the regex
engine at all — it was the **grammar-action walk's function dispatch**. On
`benchmarks/bench-yaml-parse.raku` (release, clean idle box), main went
3.95s → **1.73s** across PRs #5573/#5574/#5575, vs raku 0.25s on the same box:

- Reduce-walk `Arc::make_mut` deep copies: 40,297 → 0 (#5573; counted, no
  wall-clock change — ADR-0016 correctly predicted this class was noise-level).
- Embedded-code parse cache (`REGEX_CODE_PARSE_CACHE`, #5574): `{…}`/`<?{…}>`/
  `** {code}` strings parsed once per registry generation.
- **Negative function-resolution gate** (#5574, the big one, −45%): the action
  walk resolved `make` (80,240×) and `prefix:<~>` (38,418×) through the full
  registry-scanning `resolve_function_with_types` walk, failing every time
  (neither is a registered function). A per-name "does any registry key carry
  this base name?" memo (`fn_base_name_cache`) short-circuits it.
- **Precise `fn_resolve_gen` bump** (#5575, −16%): the interpreter-fallback
  call arm bumped the gen after EVERY native-builtin call, clearing all
  name-keyed call caches once per `make`; now it bumps only when the call
  actually acquired a registry write guard. This exposed (and #5575 fixes) a
  latent bug the churn had been masking: the frameless fast/light call paths
  ran `callframe`/`CALLER::`-using bodies without pushing a caller frame
  (new `CompiledCode::uses_callframe` compile-time gate; pin
  `t/source-line-table.t`).
- Leaf/interior early-exits in `invoke_grammar_actions` (#5575): childless
  no-action nodes skip the attribute-map clones, node rebuild and env
  save/restore ceremony entirely.

Remaining (next session): the action-method call ceremony
(`call_method_with_values` → `run_instance_method`) is ~50% inclusive, and
the ADR-0016 P2+ structural work (CapNode split, spans, lazy Match) still
stands for the allocator/memcmp tail.

(A profile taken right after a rebuild shows `load_module`/`parse_program` at
~19% — that is the **cold first run only**: the disk precomp cache
(`src/precomp.rs`) keys on the executable's mtime, so every fresh build parses
each module once and re-caches. Measured warm vs `MUTSU_PRECOMP=0` on the
bench: 1.72s vs 1.82s, i.e. the whole parse+cache path is ~0.1s (~6%) per
cold run and ~0 warm; `use YAMLish` alone is 0.02s warm / 0.04s uncached.
Precomp covers the AST only — token/rule regex-slang bodies still parse
per process at match time into the in-memory `REGEX_PARSE_CACHE` — but that
cost is inside the ~0.1s, so on-disk regex precomp is NOT a promising lead.)

The YAML battery is correct — all 5 upstream files (81/81 subtests) pass — but it
is **slow**, and the cost is in *matching*, not module load. This is the
match-time twin of `grammar-heavy-module-load-slower-than-raku.md`; that ticket
measures `use`, this one measures the parse itself.

**Three rounds of this have already landed** (see the important measurement
caveat right after them — most of this session's local A/B numbers turned out
to be unreliable).

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

Round 3 (`news/2026-07/match-object-orig-arc-share.md`): `make_match_object_full_q`
(the Match-object builder used by grammar parsing, `~~`, substitution, and
`split`) re-derived the `.orig` string and the position-search haystack from
scratch — `orig.chars().collect()` and `orig.to_string()` — inside its
per-leaf-capture helpers, so a leaf-heavy match tree (again, one leaf per
matched character in a quoted scalar's space run) re-collected/re-cloned the
*entire original document* once per leaf. Both are now computed once per
`.parse()`/match and shared via `Arc` (`Value::str_arc`) and a borrowed
`&[char]` slice instead. This is a correct, tested, real reduction in
allocation — but see the measurement caveat below for why its wall-clock
contribution could not actually be confirmed this session.

## Measurement caveat: a 3-day-old CPU-spinner corrupted this session's local A/B

Partway through round 3, `ps -eo pid,pcpu,comm --sort=-pcpu` turned up eight
`sh -c 'for i in $(seq 1 8); do (while :; do :; done) & done; sleep 12; kill
...'` processes (PIDs 694193-694201), each pinned near 73% CPU, running
continuously since 2026-07-25 — over 2 days. The `sleep 12; kill` clearly
intended to self-terminate but the backgrounded `while :; do :; done` loops
outlived it (job-control artifact of a non-interactive shell), leaving ~6
cores' worth of pure busy-loop competing with everything else on a 12-thread,
hybrid P-core/E-core (13th Gen i7-1355U) laptop chip for 3 days straight.

This made every local wall-clock and even `perf record` sample-count
comparison in this session **unreliable**, not just noisy: on a hybrid CPU,
which core type (fast P-core vs slow E-core) the scheduler assigns a run to
under contention swings effective speed by 2-4x on its own, independent of
any code change. Concretely: the "round 2 roughly halves total samples"
finding cited above (and in its own news entry) was measured under this
contention and looked like a ~2x win; after killing the spinners and
re-measuring the SAME two binaries (pre-round-2 vs round-2+3) pinned to one
core (`taskset -c 0`), both took the same ~17.5s task-clock for
`basic.rakutest`'s worst subtest, and a plain (unpinned, but now
spinner-free) comparison on `dump_n9.raku` (the `todo/tickets/...`
reproduce-script case at n=9) also showed no measurable difference
(~6.0-6.5s both ways over 3 runs each). Rounds 2 and 3 are still correct,
safe, tested fixes for real inefficiencies (verified by reading the code and
by the `perf` self-time cluster disappearing/never reappearing in a **clean**
profile — see below) — they are just smaller in absolute wall-clock terms
than this session initially believed, and **not** the dominant cost for this
document shape.

A clean (spinner-free, `-F 4000` for better sample resolution) `perf record`
self-time sort of `dump_n9.raku` under the round-2+3 binary shows **no single
dominant function** — `malloc` leads at under 9%, with `__memcmp_avx2_movbe`/
`_int_free`/`cfree`/`_int_malloc`/`__memmove_avx_unaligned_erms` close behind
and a long tail of small (`~1%` or less) mutsu functions. This is "death by a
thousand small allocations" rather than one fixable call site — consistent
with, but now actually *confirmed* rather than assumed, item 4 below.

**Lesson for future sessions on this box**: before trusting ANY local A/B
timing (wall-clock OR `perf` sample counts) here, run `ps -eo
pid,pcpu,comm --sort=-pcpu | head` and look for anything pinned near a fixed
high `%CPU` for an implausibly long `TIME`/elapsed — a busy-loop artifact
looks exactly like that, is easy to miss among normal `cargo`/`rustc` churn,
and (as demonstrated here) can silently invalidate an entire session's
"confirmed" perf findings. `taskset -c <core>` reduces (not eliminates)
remaining variance from thermal/frequency scaling once the machine is
actually idle.

**A `benchmarks/bench-yaml-parse.raku` file now exists** (this exact
space-heavy block-sequence shape, sized for CI's `BENCH_TIMEOUT`), so future
rounds on this ticket get tracked automatically in `bench-history.tsv` on the
`bench-data` branch — use that history, not a local run, to judge whether a
future change actually helped.

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
4. **Residual `malloc`/`_int_free`/`__memcmp_avx2_movbe` cost — confirmed, not
   just hypothesized, on a clean machine (see the measurement caveat above).**
   After rounds 2+3, these plain allocator/libc symbols are still the largest
   entries in a `perf` self-time sort of the space-heavy synthetic, and no
   single mutsu function stands out (each is ~1% or less). This is genuine
   O(n) `Match`/capture object construction — one per matched space
   character, since each is its own quantified `<str=space>` capture, and
   each such capture builds a fresh `HashMap` (str/from/to/list/named/orig)
   plus a GC-managed `Instance` via `Value::make_instance`. Reducing this
   is NOT a single call-site fix like rounds 2/3 — candidates worth
   investigating, roughly in order of invasiveness:
   - Avoid building a full `Match` **object** for a quantified list element
     that is never actually accessed as a `Match` (only `.Str`-ified) — hard
     to prove safely in general since Raku lets any element be inspected.
   - Reduce the *shape* of the per-element `HashMap`/`Instance` (e.g. a
     smaller specialized repr for "leaf capture, no subcaps" instead of the
     same general Match shape as a top-level richly-nested one).
   - Look at `RegexCaptures` cloning in the `Alternation` match-merge loop in
     `regex_match_atom.rs` (`regex_match_ends_from_caps_in_pkg`) — every
     `<str=single-bare> | <str=single-quotes> | ...` alternative attempt
     clones `RegexCaptures` (named/positional maps) on each candidate tried,
     which is itself allocation-heavy per position even before a `Match`
     object is ever built.
   This is a different, deeper investigation than any of rounds 1-3 — treat
   it as its own slice, and validate with `benchmarks/bench-yaml-parse.raku`
   via `bench-data` (not a local run) once a candidate fix exists.

   **Done (2026-07-28): that investigation produced
   `docs/adr/0016-span-based-captures-and-lazy-match.md`.** It names five
   structural causes and commits to spans-into-a-shared-subject plus a lazily
   materialized `Match`, phased P1-P5. Two findings correct the guesses above:
   - The biggest copy source was not the `Alternation` merge loop (that path
     already `drain`s rather than clones, post-ADR-0007). It was the **subrule
     re-slice**: matching a subrule body against `&chars[pos..]` made every inner
     offset slice-relative, and the rebase (`shift_capture_descendants`) went
     through `Arc::make_mut` on `Arc`s that `record_reduced_subrule` had already
     shared — so every subrule call deep-copied its whole descendant subtree, at
     every nesting level, for every candidate. **P1 removed it** (see
     `news/2026-07/regex-subrule-absolute-positions.md`), and fixed three
     look-behind/word-boundary compatibility bugs on the way.
   - The 11% `__memcmp_avx2_movbe` is two things, not one. A gated experiment
     (skip the leaf-capture position search, change nothing else) moved it to
     6.9% at equal wall clock: ≈4 points are the Match builder recovering leaf
     offsets by **searching the subject for the captured text** (it does this even
     for positional captures, whose exact spans are already recorded in
     `positional_offsets` and simply ignored — ADR-0016 P3). The remaining ≈7% is
     owned `String` capture-name keys compared on every `HashMap` probe and trail
     record (P4: intern them as `Symbol`).

   Remaining phases are tracked in the ADR, not here.

## Why it matters

A bundled battery is loaded and *used* on every run of a program that needs it.
Reading a 100-line config file must not take seconds.
