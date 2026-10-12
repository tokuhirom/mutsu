# ADR-12917: Where a grammar parse with actions spends its time, and what to change

- **Status**: Proposed (2026-10-12). Measurement and ranking only; each direction lands as its
  own slice under #12917.

## Context

`benchmarks/bench-yaml-parse-big.raku` (YAMLish, 60 rows) is the series #12917's close
condition is stated on: `bench-yaml-parse-big@section` at or under rakudo. On 2026-10-12
(`bench-history.tsv`, commit 14568bcb) it was 0.1977 s against rakudo's 0.0305 s, **6.48x**.
Reaching the goal needs a ~6.5x speedup of the warm (second and later) `load-yaml` call. Under
the perf-tuning rule "state the required multiple first", single-digit-percent slices cannot get
there; the directions below are ranked by how much of the steady-state cost each one can
remove.

### Measurement

`--profile profiling` build, `MUTSU_JIT=off`, callgrind. The steady state is the difference
between a script that calls `load-yaml` four times and one that calls it once, divided by
three, so module loading, rule compilation and first-use caches drop out. Base: #12979 (the
first slice below) on top of `main` at 751e7863.

**One warm `load-yaml`: ~681M instructions.**

| inclusive per parse | what |
|---:|---|
| ~343M (50%) | the regex engine (`rx_run_in`) |
| ~326M | — of which the eager subrule path (`subrule_candidate_ends`): every end of a callee computed up front, through the growing-seed loop; 190 such calls per parse, all with verdict `lr-seed` |
| ~67M | — `rx_call_resolve`: per-call name resolution, ~121k calls (`Symbol::lookup` plus several memo probes per call) |
| ~105M | — LTM ranking runs (`ltm_measure_roots` 56M, `LtmNfa::run` 49M) |
| ~39M + ~38M | — re-parsing interpolated rule bodies (`parse_regex`) and rebuilding their NFAs (`NfaBuilder::build_pattern`), *in the steady state* |
| ~341M (50%) | the post-parse action walk (`invoke_grammar_actions`) |
| ~296M | — calling the action methods (`call_method_with_values_inner`), ~5,800 calls |
| ~110M | — `.new` from the actions (244 constructions) |
| ~81M + ~52M | — replaying superseded reductions (`flush_deferred_repeats`, `replay_reduce_action_entries`) |

Wall clock on the same build: `Grammar.parse` 0.300 s, of which the parse without actions is
0.066 s.

### The root cause behind the largest regex row

All 190 eager calls are YAMLish's `$indent`-parameterized rules (`block`, `map`, `map-entry`,
`element`, `block-ws`). They go eager because `subrule_cannot_left_reenter`
(`regex_left_call_graph.rs`) cannot prove them free of left recursion, for two reasons:

1. a call with arguments answers "unknown" in `left_calls_of_atom`, although its rule name is
   static and the arguments only pick among that name's candidates; and, decisively,
2. `rule_body_edges_are_generation_stable` is false for any body whose text interpolates a
   variable (`$indent`): mutsu splices a `$`-valued scalar into the pattern **source** and
   re-parses it per call (`interpolate_regex_scalars`), and a `Regex`-valued scalar spliced
   that way really can add a call edge. So the body's call edges are unknowable before the
   value is.

Fixing (1) alone changes nothing (measured: still 190 eager calls). The same textual splicing is
also why the body is re-parsed and its NFA rebuilt on every call (the 39M + 38M rows), and why
the call resolution has to re-derive per call what a static call site could cache.

Rakudo compiles `$var` in a regex to an interpolation node: a `Str` value matches literally, a
`Regex` value is called as a subrule. The pattern's structure, and so its call edges, never
depend on the value.

## Decision

Rank the directions by the steady-state share they can remove, and land them in this order:

1. **Construction from actions — landed as the first slice (#12979).** `.new` re-derived class
   shape per construction (role submethod ordering cloning whole `RoleDef`s, the QuantHash base
   walk, smiley rules). Answered from `NativeCtorPlan`: 1,291M → 971M Ir for the whole
   benchmark process (-24.8%), `dispatch_new_unallocated` 395M → 83M.
2. **Compile `$var` interpolation in a regex as a runtime atom, never as spliced source.** A
   `Str` value matches as a literal (as the current quoting already makes it), a `Regex` value
   is entered as a call frame. Then a rule body's parse, compiled program, NFA and call edges
   are value-independent; `rule_body_edges_are_generation_stable` holds for such bodies, the
   left-recursion proof goes through for YAMLish's rules, and their calls run as lazy frames.
   Expected to remove most of the ~326M eager-path row plus the ~77M re-parse/rebuild rows
   (together ~55-60% of a parse). Together with it, let an argument-bearing call be a static
   edge in the left-call graph (reason 1 above).
3. **Per-call-site call resolution.** Cache the whole `ResolvedCall` verdict of an
   argument-less call on its `NamedAtom` (keyed by package, `:i` and the token generation),
   in front of the per-layer memos, instead of re-running every layer per call. Up to ~67M
   (~10%).
4. **Action method calls as compiled-closure calls.** The ~5,800 action dispatches go through
   `call_method_with_values` with name lookup, argument binding and env setup per call. Resolve
   the action method once per (actions class, rule name) and invoke it as a compiled method
   with a cheap frame. Bounded by the ~296M row minus the user bodies' own work.
5. **Superseded-reduction replay.** `flush_deferred_repeats` partitions the whole pending list
   at every node (O(nodes x pending)); keep it ordered so a flush is a prefix pop. Up to ~30M of
   overhead; the replayed actions themselves (rakudo runs them too) stay.

Directions 2 and 4 are the two that can each remove a large fraction of the remaining cost;
the goal (6.5x from the original) is out of reach without both.

## Rejected alternatives

- **Trimming per-layer costs first** (more memos inside `rx_call_resolve`, faster
  `Symbol::lookup`). Each layer already carries a memo; the perf-tuning skill's §5a records
  where that converges. Direction 3 replaces the layers' per-call work instead.
- **A YAMLish-specific path** (recognizing its grammar, or providing YAMLish natively). Banned
  by ADR-0096, and it would not help any other grammar that threads parameters through rules.
- **Proving left-recursion freedom for interpolating bodies by special-casing `Str`-typed
  parameters** (`token block(Str $indent)`). A parameter's declared type does not bind every
  variable a body interpolates, and the splice would still re-parse per call; direction 2
  removes the cause for every body at once.

## Consequences

- Direction 2 changes how every interpolating regex is compiled; it is a regex-lane campaign
  slice of its own, verified on roast `S05-*` and the ecosystem grammars, not a perf tweak.
- Each later slice re-measures the steady state the way this ADR does (four calls minus one)
  and records the result in an Outcome section here.
