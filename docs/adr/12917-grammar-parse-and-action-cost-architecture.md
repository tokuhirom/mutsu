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

## Outcome

### Direction 2 (2026-10-12): rule parameters bound per call

Landed for a rule whose body is a regex literal and whose parameters are plain
positional scalars, the only shape YAMLish uses (`src/runtime/regex/regex_rule_params.rs`).
The resolver prefixes the body with a parameter declaration block, the pre-pass and the
structural parser lower `$param` to a match-time `VarInterp`, and the call's binding window
holds the values. `VarInterp` enters a `Regex` value as a frame and honours `:i`. The
left-call graph takes such a rule's body as its node and a call with arguments to it as an
edge. A proto or multi rule with parameters, a defaulted, coerced, `where`-constrained or
slurpy parameter, and an argument that binding would itemize (an `Array`, a `Hash`, an
object) keep the spliced-source path.

`regex-eager` went from 190 (all `lr-seed`) to 0 per `load-yaml`. The steady state did
**not** move: 646M Ir per parse before, 648M after (release build, `MUTSU_JIT=off`, four
calls minus one, divided by three). The ~326M "eager subrule path" row of the table above
was inclusive of the callees' own matching, which a lazy frame performs as well; the
growing-seed bookkeeping itself was small. The ranking of direction 2 was wrong for that
reason; its value is the architecture (one body per rule, rakudo's interpolation
semantics), not speed.

The same profile, read by inclusive cost under `rx_run_in` (~341M per parse), shows where
the regex half goes instead:

| inclusive per parse | what |
|---:|---|
| ~275M | look-behinds (`RxOp::Look`, `is_behind`) trying every start from 0: `lookbehind_start_floor` cannot bound a body that calls a rule, so YAMLish's `<!after <.space>>` (in `plain`, once per character) scans the whole document prefix each time — the superlinear term the benchmark header measures |
| ~51M | `rx_call_resolve` (direction 3) |
| ~64M | LTM ranking |
| ~19M | `parse_regex` in the steady state |

So the next slice is a new direction ahead of 3-5: **bound a look-behind through the rules
it calls** (a static, argument-less rule body has a bounded width just as an inline class
does). It removes most of the ~275M and the quadratic growth with document size. The
action walk (~306M, direction 4) is then the largest remaining item.

### Look-behind bound through rule calls (2026-10-12, #13000)

`lookbehind_start_floor_in` (`src/runtime/regex/regex_lookbehind.rs`) now bounds a call to
an argument-less rule by the longest of its candidates' bodies (up to eight calls deep), and
a builtin class such as `<.alnum>` by one character. One warm `load-yaml` of the 60-row
benchmark document went from ~646M to ~294M Ir (-54%; release, `MUTSU_JIT=off`), the section
time here from ~0.165 s to ~0.108 s, and growth with document size became linear (640 rows:
6.46 s to 1.45 s).

The re-profile on top of it (the `profiling` cargo profile, whose absolute counts run higher
than the release figures above) puts the action walk first: inside `Grammar.parse`,
`invoke_grammar_actions` is ~80% of the inclusive cost and the regex engine's own matching
(`rx_match_ends`) ~22%, with `.new` of the result objects a large share of the actions.
Direction 4 (the action walk) is the next slice.

### Direction 4, first slice (2026-10-12): the walk's funnels

The `profiling` build on top of #13000 measured **~414M Ir per warm `load-yaml`**
(four calls minus one, `MUTSU_JIT=off`); the action walk (`invoke_grammar_actions`,
toggle-collected) was ~260M of it. A micro-benchmark of one action
(`method c($/) { make ~$/ }` over 2,000 characters) put the parse at ~4k Ir per
character and the action at ~79k, of which the body's own work was a few thousand:
the rest was the general method funnel in front of the compiled method (~25k), the
function-call funnel in front of `make` (~15k) and the call frame. Four changes:

| change | per warm parse |
|---|---:|
| a role attribute default's captured scope shared (`Arc`) instead of copied with every attribute list (`.new` cloned and dropped ~84k `String`s per parse); landed concurrently by #13012 (#12981), #13022 only shares the role's own copy too | 414M → 358M |
| action methods resolved directly (`call_grammar_action_method`); unshadowed `make` straight to the builtin; an empty deferred-repeat flush answered by a scan | 358M → 323M |
| `$<name>` read as the named capture, like `$/<name>`, not through the `AT-KEY` funnel (~45k → ~7.6k Ir per read) | 323M → 290M |

Together **~414M → ~290M (-30%)**; #13022's own share, measured against `main` with #13012 (2b75985b), is **~359M → ~290M (-19%)**. What remains of the walk is spread thin: the
compiled-method call frame (~10k Ir per call: env merge, frame push/pop, attribute
reconcile), the per-action env ceremony (`take`/`restore_action_named_captures`,
~6k), `.new`'s per-construction class-shape work (`collect_class_attributes`,
`collect_attribute_type_constraints` three times, the attribute-default scope
inserted into the env per default), hyper method calls (`».ast`) and `~$/`
re-resolving `prefix:<~>` per call. None of these is above ~3% alone; the
remaining factor to rakudo (~2.5x by this estimate) needs, above all, the call frame itself
to get cheaper, which is largely the env campaign's static link (ADR-12529) rather than a
grammar-side change.
