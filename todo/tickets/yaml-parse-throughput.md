# Parsing YAML with the bundled `YAMLish` is still ~5-35x slower than raku

**Update (2026-08-14, round 7): `basic.rakutest` is fixed (round 5's "barely
improved" concern is resolved, ~7x faster now) and `MUTSU_VM_STATS` traces the
post-P5 drift to a concrete, falsifiable mechanism — near-total loss of P5's
Match laziness — but the exact call site that grew is not yet found.** This
round picks up round 6's two open items ("Where to look next" item 5) with two
isolated worktrees: `origin/main` tip (`96dbb964a`, later rebased to
`744c0e340` for the doc-only PR) and the pinned P5-merge commit `fa2400a49`.

**Item 1 — VM_STATS comparison.** Built `cargo build` (debug, per CLAUDE.md:
these counters are optimization-level-independent) in both worktrees and ran
`MUTSU_VM_STATS=1 target/debug/mutsu benchmarks/bench-yaml-parse.raku`
identically in each (same script content in both trees, byte-identical). Key
counters, P5-merge (`fa2400a49`) vs current (`96dbb964a`):

| counter | P5-merge (`fa2400a49`) | current (`96dbb964a`) | delta |
| --- | ---: | ---: | ---: |
| `regex-captures: leaf_spans` | 26 | 19226 | **×739** |
| `regex-captures: match_materializations` | *(counter didn't exist yet)* | 20951 | n/a |
| `gc: collections` | 4 | 3 | -1 |
| `gc: pause_ns_total` | 112958599 (113ms) | 232557240 (233ms) | ×2.06 |
| `gc: pause_ns_max` | 61978635 (62ms) | 134062743 (134ms) | ×2.16 |
| `dual-store: clone_env` | 40221 | 40221 | 0 |
| `dual-store: env_deep_copies` | 41348 | 41388 | +0.1% |
| `function-call opcodes` / fallback% | 40161 / 100.0% | 40161 / 100.0% | 0 |
| `method-call opcodes` / fallback% | 5120 / 48.4% | 5120 / 48.4% | 0 |
| `jit: entries` | 38689 | 38689 | 0 |
| `opcodes executed total` | 74608 | 74943 | +0.4% |
| resolver-path dispatch `space=` (leaf-token count) | 35328 | 35328 | 0 |

The picture this narrows down: dual-store, JIT, opcode-execution volume, and
the grammar engine's own subrule-dispatch workload (`space=35328` identical in
both — the same document produces the exact same number of leaf-token match
attempts) are all flat. The regression is **narrowly localized to the
regex-captures/Match-laziness subsystem**: `leaf_spans` (a counter that
existed at the P5-merge commit too, so this is a same-counter, not a
newly-added-counter, comparison) exploded ×739 for the byte-identical
benchmark, and the brand-new `match_materializations` counter (added after P5,
so no baseline exists) shows 20951 forced materializations — i.e. nearly every
leaf capture in the match tree is now being fully materialized into an
eager `Instance`-shaped attribute map, which is exactly the behavior P5's
"lazy Match" (`docs/adr/0016-span-based-captures-and-lazy-match.md`) was
built to avoid. `gc: pause_ns_total`/`pause_ns_max` roughly doubling is
consistent with (and plausibly downstream of) that much extra allocation —
though pause-time counters are wall-clock-based, not pure op counts, so
treat them as directional only, especially since the box was moderately
loaded during this run (see below).

**This is real, not a measurement artifact of turning `MUTSU_VM_STATS` on**:
the counting itself (`record_regex_match_leaf`/`record_regex_match_materialization`
in `src/vm/vm_stats.rs`) is gated behind `vm_stats::enabled()`, but the
*counted event* — `MatchNode::force_attrs()`/`materialize_map()` in
`src/value/match_lazy.rs` actually running — happens unconditionally in
production; only the bookkeeping is skipped when stats are off.

**Chasing the call site (partial, not concluded):** a `rust-gdb -batch`
breakpoint at `match_lazy.rs:74` (inside `force_attrs()`'s memoizing closure)
on the current build showed the first 3 hits all routing through
`Value::view()` → `OpCode::GetGlobal`'s LazyThunk probe at
`vm_exec_dispatch.rs:532` (`if let ValueView::LazyThunk(thunk_data) =
val.view() { ... }`), called unconditionally on every `GetGlobal` read to
check "is this a thunk" — forcing full Match materialization even when the
value obviously isn't a thunk. Two sibling sites do the same thing:
`exec_get_upvalue_op` (`vm_var_assign_local_get.rs:58`, `GetUpvalue`) and the
SetLocal readonly-marking check (`vm_var_assign_set_local.rs:1817`). By
contrast, `exec_get_local_op` (`GetLocal`, `vm_var_assign_local_get.rs:236`)
already guards the same check behind the cheap `is_lazy_thunk_value()` tag
probe first, with a comment explicitly noting "a `view()` would materialize a
lazy Match" — i.e. someone already fixed this exact class of bug for
`GetLocal` but not for `GetGlobal`/`GetUpvalue`/the `SetLocal` check.

**However: this specific inconsistency predates the regression window.**
Diffing all four call sites against the `fa2400a49` (P5-merge) tree shows them
byte-identical — `GetLocal` was already guarded and `GetGlobal`/`GetUpvalue`/
the `SetLocal` check were already unguarded *at the P5 merge itself*. So while
this is a real, independently-worth-fixing inefficiency (any Match value read
via `GetGlobal`/`GetUpvalue`, or stored via a guarded `SetLocal`, pays an
unnecessary full materialization), it cannot by itself explain the
26→19226 delta — the code paths did not change. `GetGlobal` itself also only
executes 49 times total in this benchmark (per the opcode histogram, same in
both builds), far too few to produce ~20000 materializations on its own; the
3 gdb samples captured were just the first few hits, not a representative
sample of the dominant contributor(s).

**What actually changed is still open — the concrete next step for round 8:**
something in the ~1700-commit window caused many more Match values to reach
one of these unguarded (or another, not-yet-found) `.view()`-forcing site than
before. The prime suspect, not yet verified: `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`
(ADR-0019), whose E3/E4/F1 phases (generation-keyed resolved-sequence caches,
`NativeCallBinding` resolution, `user_candidates` cutover for
`class_method_table`/`collect_can_methods`) landed heavily in exactly this
window and reworked method/dispatch resolution — precisely the kind of change
that could add a new `.view()` call on a method receiver (a Match value, for
`AT-KEY`/`.Str`/action dispatch on capture nodes) where none existed before.
Round 8 should run a gdb hit-count *sweep* (not 3 samples: use `ignore N` to
skip past the first batch, or an env-gated backtrace-on-every-Kth-hit) to find
the call site(s) that actually dominate the 20951 total, then check whether
they trace to an ADR-0019 landing.

**Item 2 — `basic.rakutest` re-measurement.** Built `cargo build --release`
in the current-main worktree and ran the file 4× (fetched copy at
`tmp/battery-testsuite/YAMLish/t/basic.rakutest`, same file referenced by
rounds 1-5):

| run | wall time |
| --- | ---: |
| 1 | 6.881s |
| 2 | 6.027s |
| 3 | 6.032s |
| 4 | 6.193s |

All 4 runs: 7/7 subtests passing (clean TAP, `1..7`, zero `not ok`). Median
~6.1s — down from round 1's **43.6s** baseline, a **~7.1x** wall-clock
reduction. Against round 1's raku reference for this same file (1.2s), the
mutsu/raku ratio on `basic.rakutest` is now ~5.1x, down from ~36x at round 1.
**This resolves round 5's "barely improved" flag for this file** — nobody had
re-measured it since ADR-0016 P1-P5 landed, and it turns out ADR-0016 fixed
this file's dominant cost too, not just the synthetic benchmark.

Caveat: the machine was moderately loaded during these runs (`uptime` load
average 2.1-4.4 over the run window; a sibling worktree was concurrently
running its own `cargo build && cargo test && prove t/` pipeline) — not the
fully-idle box this ticket's own methodology asks for, and not a
`bench-history.tsv` CI number. But the 4-run spread was tight (6.0-6.9s,
~14%), giving reasonable confidence in the order of magnitude (7x, not e.g.
2x or 15x) even without CI-grade precision.

**Net assessment:** ticket stays open — item 1's drift is confirmed real and
now has a much narrower localization (Match-laziness/regex-captures, ruling
out dual-store/JIT/opcode-volume/grammar-dispatch-volume) plus a concrete,
falsifiable next step, but the dominant call site is not yet identified. Item
2's picture is now good news: the whole-upstream-file concern round 5 flagged
is resolved, and the mutsu/raku ratio on real-world files (~5x) roughly
matches the synthetic-benchmark ratio family, not the ~35x this ticket's title
still says (title left as-is since the *synthetic microbenchmark* ratio via
`bench-history.tsv`, per the round 6 table, is still ~35 post-drift — the
title reflects the worst still-open number, not the best one).

---

**Update (2026-08-14, round 6): ADR-0016 landed and delivered a ~4x wall-clock
win on `bench-yaml-parse`, confirmed from `bench-history.tsv` on `bench-data`
— but a real, still-open gap remains, plus a smaller unexplained drift since
the ADR merged.** This round did not change any code; it re-reads the CI bench
history (per "Benchmark numbers in documents come from the bench CI" in
`CLAUDE.md`) to check what round 5's prediction ("the next round is
structural, not another call-site fix... exactly ADR-0016 P2/P5 territory")
actually bought, now that all five ADR-0016 phases have landed (see
`docs/adr/0016-span-based-captures-and-lazy-match.md`, whose own §Phasing
already contains the P1-P5 measurements up to the P5-merge commit
`fa2400a49`; this entry extends that history two more weeks).

Joining `bench-yaml-parse`/`bench-yaml-parse+jit` against `int-arith`/
`int-arith+jit` on commit (the same int-arith-normalization method the ADR
itself used, since this benchmark has no direct raku column in
`bench-history.tsv` — `raku_median_s` is `NA` on every row, presumably because
CI's raku environment does not have YAMLish installed) reproduces the ADR's
own numbers exactly for the commits it cites (`0eb479d9`/`17af2292`/
`afc3475e` at 117.4/116.9/121.5; `fa2400a49` — the P5 merge — at 25.65), which
is a good sanity check that this method is sound. Extending it from there
(plain/`MUTSU_JIT=off` series, daily median of the joined ratio and raw
`mutsu_median_s`):

| date (2026-08) | commit (representative) | raw median (s) | ratio ÷ int-arith |
| --- | --- | ---: | ---: |
| 07-28 (pre-P2) | `0eb479d9` | 5.59 | 117 |
| 07-30 13:16 (P2 lands) | `22fb3ece8d` | 3.71 | 64 |
| 07-30 15:25 (P3a lands) | `acf00cf006` | 1.96 | 41 |
| 07-31 00:13 (P4 lands, pre-P5) | `d74fdf2e52` | 2.24 | 38 |
| 07-31 01:28 (**P5 merge**) | `fa2400a49` | **1.39** | **25.65** |
| 07-31 (day median, post-P5) | — | 1.35 | 25.4 |
| 08-01 | — | 1.38 | 25.6 |
| 08-02 | — | 1.42 | 26.6 |
| 08-03 | — | 1.41 | 31.9 |
| 08-08 | — | 1.54 | 30.8 |
| 08-12 | — | 1.56 | 32.3 |
| 08-13 | — | 1.60 | 34.4 |
| 08-14 (today, tip of `main`) | `ed3d40f59` | 1.60 | 35.1 |

(All rows above are the plain/`MUTSU_JIT=off` series. The `+jit`/default-config
series tracks the same trend but runs ~10-15% higher throughout — e.g. on
`ed3d40f59` itself, `bench-yaml-parse+jit` is 1.795s/ratio 46.4 versus the
plain row's 1.60s/35.1 — plausibly JIT compile/warmup overhead not amortized
over a single-shot parse; not investigated further here.)

Two conclusions, both grounded in `bench-history.tsv`, not a local run:

1. **The structural fix round 5 predicted was correct and large.** P2
   (`CapNode` split) and P3a (spans, not text) each cut the ratio roughly in
   half in sequence (117 → 64 → 41), P4 held that gain, and P5 (lazy `Match`)
   cut it again by another third (38 → 25.65). Peak-to-trough this is a
   **~4.5x wall-clock reduction** (5.5-6.8s down to ~1.35s), landed and
   verified the same day the ADR's own P5 entry claims (2026-07-31), not just
   a local A/B.
2. **Since the P5 merge, the ratio has drifted back up from ~25 to ~34-46 over
   the following two weeks**, and — importantly — this is not purely an
   `int-arith` normalization artifact: the *raw* `bench-yaml-parse` median
   also crept from ~1.35s (07-31) to ~1.55-1.60s (08-12 to 08-14), a genuine
   ~15-20% wall-clock regression on top of the post-P5 number, even before
   accounting for `int-arith` itself getting a bit faster over the same
   window (which inflates the *ratio*'s apparent growth beyond the raw-time
   growth). No single commit stands out as the cause: `git log --oneline
   origin/main --since=2026-07-31 --until=2026-08-14` on this repo counts
   **~1700 merged commits** in that window, spanning many unrelated
   interpreter subsystems (dispatch caches, GC, new opcodes, ADR-0019/ADR-0021
   /ADR-0022 work, etc.) — a diffuse few-percent overhead compounding across
   call/dispatch paths on a tight parse loop is a more likely explanation than
   a single regex-specific regression, but this is *not confirmed* by
   profiling. Bisecting it would need the same idle-box care as the
   measurement caveat below (this session did not attempt it — see "Where to
   look next" item 5, added this round).

**Net assessment: still open, not closeable.** The ~4.5x structural win is
real and durable (every daily median since 07-31 is at or below ~35, versus
~110-140 before ADR-0016), but the gap to raku has not closed to
"diminishing returns" — round 4 measured raku at 0.25s on the same document
shape when mutsu was 1.73s (~7x), and round 5's 1.27s (~5x); mutsu's current
CI-measured ~1.55-1.80s is *worse in absolute terms* than round 5's local
1.27s, even though the underlying representation is now the ADR-0016 one.
Until either a fresh idle-box local A/B against `raku` is taken, or CI grows a
raku column for this benchmark, the live ratio-to-raku is not precisely
known — but "close enough to stop" is not supported by any number gathered
this round. This ticket stays open under `todo/tickets/`.

The whole-upstream-file table below (`basic.rakutest`, unmoved by rounds 1-5
per round 5's own admission) has not been re-measured post-ADR-0016 either —
doing that (on an idle box, or via a future `bench-data` addition) is the
other open item.

---

**Update (2026-07-31, round 5): ~5x now.** The action-walk call ceremony named
by round 4 broke down into three independent per-call costs, all removed
(`benchmarks/bench-yaml-parse.raku`, release, clean idle box: 1.70s → **1.27s**,
−25%; the 5 upstream YAMLish files run in ~11s total, was ~26s):

- **`~$<key>` Str coercion of a Match paid a two-stage method-dispatch
  ceremony** (13.9% of the profile): `exec_str_coerce_op` routed every Match
  instance through `try_compiled_method_or_interpret("Stringy")` →
  `call_method_with_values` → `should_bypass_native_fastpath` (a ~20-arm
  predicate chain) → `run_instance_method` → the native handler that finally
  read the `str` attribute. A grammar action does this once per capture. Now a
  plain `Match` (no user augment of `Match.Str`/`Stringy`, no `prefix:<~>`
  overload) reads the `str` attribute directly in the opcode.
- **Every `make(...)` call re-scanned the whole functions registry twice**
  (~10%): the lexical-`&name`-override gate in `exec_call_func_op` called the
  uncached `has_multi_candidates` (full functions-map key scan), and
  `normalize_call_args_for_target` called `has_multi_function` (same scan) plus
  `has_declared_function` — for a name (`make`) that is not a registered
  function at all. Both sites now short-circuit through the #5574
  `fn_base_name_registered` negative gate (and the override gate checks the
  cheap `!has_function` before the memoized `has_multi_candidates_cached`).
- **`has_public_accessor` cloned the class's whole merged attribute list per
  query** (~4%, on the per-call dispatch path via `call_method_with_values`):
  `collect_class_attributes` builds and clones a deduplicated
  `Vec<ClassAttributeDef>` across the MRO. The query now walks the MRO
  derived-first and stops at the first class declaring the name — same
  override-by-name semantics, no clones.

Remaining leads — **corrected after a post-merge re-profile** (unprivileged
`perf`, warm cache, so no sudo/cold-cache contamination): the earlier
"ceremony between `call_method_with_values` and the JIT body is 15-20 points"
reading, and any post-merge entries like "`check_eval_param_type_constraints`
18%" / "main-script `parse_program` 11%" / "`exec_use_module_op` 16%" in a
*children* view, are **stack-truncation misattributions**, not real costs:
grammar matching recurses hundreds of frames deep, the kernel caps recorded
call stacks at `perf_event_max_stack` (127), and the severed lower fragments
get re-rooted onto whatever shallow frame they landed under. Verified with
gdb hit counters: `collect_declared_type_names_with` runs **once** per
process (the pre-run check of the 23-line bench script) and warm `use
YAMLish` is 0.03s wall — neither can be 18%/16% of a 1.3s run. **Trust
self-time (`--no-children`) and gdb hit counts for grammar-heavy profiles;
treat every deep-recursion children percentage as suspect.**

What the trustworthy self-time view actually shows (post-round-5, 1.27s):
a long tail dominated by **Match-object construction** — the
`make_match_object_full_q`/`make_subcap_match` recursion with its
`Vec::from_iter`/`make_instance`/`AttrMap::from` cluster is the largest
coherent block (≥20% even before un-truncating), over an allocator floor of
`malloc`+`cfree`+`malloc_consolidate`+`memmove` ≈ 23%. That is exactly
ADR-0016 P2/P5 territory (CapNode split, spans, lazy Match) — the next
round is structural, not another call-site fix. Two smaller leads:
`Interpreter::new` ≈ 80ms one-time startup of every mutsu invocation
(separate from match time), and process startup (`_dl_relocate_object` +
loader ≈ 6-8% of a 1.3s run).

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

   **Update (2026-08-14, round 6): all five phases (P1-P5) have now landed**
   (`docs/adr/0016-span-based-captures-and-lazy-match.md`'s own status line),
   confirmed independently from `bench-history.tsv` — see the round 6 entry at
   the top of this file. This item is done; the ADR itself is the record of
   what it fixed.

5. **(New, round 6; updated round 7) Two items neither round 1-5 nor the ADR
   closed:**
   - **Bisect the post-P5 drift — partially traced, not closed (round 7).**
     `bench-yaml-parse`'s CI-measured raw median rose from ~1.35s
     (2026-07-31, the P5-merge day) to ~1.55-1.80s (2026-08-12 through 08-14)
     over ~1700 unrelated merged commits — see the round 6 table above. Round
     7's `MUTSU_VM_STATS` comparison (P5-merge `fa2400a49` vs current) found
     the driver's *neighborhood*: `leaf_spans` exploded ×739 (26→19226) and
     the new `match_materializations` counter shows 20951 forced
     materializations, while dual-store/JIT/opcode-volume/grammar-dispatch
     counters are all flat — i.e. P5's lazy-Match win is being defeated
     somewhere, not a broad diffuse overhead. A `rust-gdb` breakpoint at
     `MatchNode::force_attrs()` (`src/value/match_lazy.rs:74`) found *a* real,
     unguarded `Value::view()` call that forces materialization
     unconditionally (`OpCode::GetGlobal` at `vm_exec_dispatch.rs:532`, and
     siblings `GetUpvalue`/`SetLocal`-readonly-check) — contrasted with
     `GetLocal`, which already guards the same check behind a cheap
     `is_lazy_thunk_value()` tag probe. But diffing those exact sites against
     `fa2400a49` shows them byte-identical, so this specific inconsistency
     predates the regression window and is not (solely) the cause of the
     delta. **Round 8: run a gdb hit-count sweep** (not 3 samples — `ignore N`
     or an env-gated backtrace-on-every-Kth-hit) across all
     `force_attrs()`/`materialize_map()` callers to find which one(s)
     actually dominate the 20951 total, and check whether they trace to one
     of the many ADR-0019 (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`)
     E3/E4/F1 dispatch-resolution PRs that landed in this exact window (see
     the round 7 entry at the top of this file for the full reasoning).
   - **Re-measure `basic.rakutest` and the other whole-upstream-file numbers —
     done for `basic.rakutest` (round 7), resolved.** Round 1-5's own numbers
     predated P1-P5 entirely; round 5 explicitly flagged `basic.rakutest` as
     "barely improved" by rounds 1-3 and pointed at exactly this structural
     work as the next lever. Round 7 re-ran it against a `cargo build
     --release` of current `main` tip: 4 runs of 6.0-6.9s (median ~6.1s),
     down from round 1's 43.6s baseline — a ~7.1x reduction, all 7/7 subtests
     passing. See the round 7 entry at the top of this file for the full
     numbers and the machine-load caveat (not a fully idle box, not a
     `bench-history.tsv` CI number, but a tight enough 4-run spread to trust
     the order of magnitude). The other whole-upstream files from the round-1
     table (`anchor-alias.rakutest`, `p5-tests.rakutest`, `roundtrip.rakutest`,
     `test-harness.rakutest`) were NOT re-measured this round — still open for
     a future round if a fuller picture is wanted, though `basic.rakutest`
     (the largest/most feature-dense file, and the one round 5 flagged) was
     the one that mattered most.

## Why it matters

A bundled battery is loaded and *used* on every run of a program that needs it.
Reading a 100-line config file must not take seconds.
