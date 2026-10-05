# ADR-7576: Dispatch grammar actions at subrule reductions

- **Status**: Proposed
- **Issue**: [#7576](https://github.com/tokuhirom/mutsu/issues/7576).
- **Relates to**: [ADR-0009](0009-regex-code-assertion-execution-model.md)
  (observable code runs only in the real match),
  [ADR-0016](0016-span-based-captures-and-lazy-match.md) (span captures and lazy `Match`),
  [ADR-0135](0135-regex-compiles-to-a-backtracking-program.md) (the regex VM and
  its subrule frames). This proposal changes ADR-0135's assumption that ordinary
  grammar actions run after matching; it does not change the regex instruction
  representation or ADR-0009's LTM boundary.

## Context

The regex VM is no longer the main cost of an action-heavy grammar. In the
`bench-data`'s `bench-history.tsv` row for `bench-yaml-parse-big@section` at main commit
`5c34451bc2d8fbfadf2cca0cc091e7631acc5a62`, mutsu took 208.3 ms and
Rakudo 31.7 ms (same runner, seven samples, ratio 6.57). Reaching #7576's
goal of a ratio at most 1.0 requires an 84.8% wall-clock reduction from that
point. The VM's `rx_run_in::<true>` self cost was about 6% in the subsequent
Callgrind profile; tuning its dispatch loop cannot supply that reduction.

The completed copy-reduction change (#11948) lowered one warm 60-row parse
from 1,640,543,603 to 1,279,183,727 Callgrind instructions and from
1,466,690 to 1,083,441 allocator calls. It did not establish Rakudo parity.
About 479,000 string-clone allocations still came from hash-map copies in the
profile. These are **whole-process local instruction and allocation counts**,
not a same-runner wall-clock comparison; the next bench CI row is needed to
measure #11948's effect on the ratio.

Today `invoke_grammar_actions` traverses the completed `Match` tree, restores
each rule's environment, calls its action, and writes updated children and
`.made` back into rebuilt parent views. A successful parse may also replay
earlier, backtracked reductions from `ReducedSubruleLog`. A failed parse
replays reductions that no surviving tree covers. `ReduceAction` already runs
a separate action copy during matching when a later iteration needs its `$*`
writes; the post-parse walk runs that action again for its ordinary effects.
These paths exist to preserve observable order and state, but they duplicate
dispatch and make the finished tree a mutation transport.

## Proposed decision

The proposed direction is to make the actual subrule reduction the single
owner of ordinary grammar-action dispatch. The dispatch point must be an
explicit logical reduction event that runs before the next matching step that
could observe the action's effects. The action sees a lazy `Match` over the
already reduced children. Its `make` result is associated with that capture
identity, so `.made` and `.ast` on aliases and on later materialized views
read the same result. The parent capture receives the completed child result;
there is no post-parse recursive write-back of ordinary actions. The prototype
result below rejects a compiled VM frame return as that event; no authoritative
dispatch point has been selected yet.

Use a stable, parse-owned reduction record (or an equivalent representation)
to connect the action result, the capture identity, and the VM's backtrack
trail. Publishing a result must not require cloning an `Arc<CapNode>` subtree
or eagerly constructing an attribute hash for every child. A retained Raku
`Match` must continue to observe its final `.made`, including when the action
itself retained `$/`. The exact carrier and its ownership model are an
implementation question for the first prototype; it must be safe when a
retained `Match` reaches another Raku thread.

The reduction event stream replaces the successful-tree action walk, the
failure replay, and the dynamic-variable-only duplicate run. A backtrack
undoes capture membership and matching state, **not already observed user
side effects**. A losing candidate's action therefore remains observable in
the order it actually reduced. The existing log's 20,000-entry cap can be
removed only when no unbounded replay log is needed; any new input-sized
buffer must have a catchable bound.

This proposal does not introduce a new interpreter, a runtime tree-walk
fallback, or a YAMLish-specific path. Action methods continue through the
existing compiled call/VM dispatch. Declarative LTM measurement, failure
position probes, and parse/analysis-only entry points must never dispatch an
action. Ordinary executable `.parse`/`.subparse` keeps the current trust
boundary: it may run user code because the caller requested execution.

## Prototype result (2026-10-05)

The first implementation dispatched directly from `file_named_candidate` when a compiled subrule
frame returned. A same-host, same-workload 60-row Callgrind comparison increased whole-process
instructions from 3,460,449,887 on the baseline to 9,553,891,859 with the prototype (+176%). The
prototype entered `dispatch_reduced_subrule` 69,867 times; the baseline's `call_method_with_values`
had 4,258 calls across all callers. This rejects the prototype as an implementation, although it
does not by itself identify which repeated matcher path causes the extra work.

An initial table of `space` action counts from a Raku wrapper was not stable across the available
binary builds, so it is omitted rather than used as semantic evidence. A GDB trace of the prototype's
first `space` dispatch located it inside the positive lookahead for YAMLish's `<break>` rule. Raku
also runs actions inside positive lookahead, so suppressing every lookahead action would be wrong.
The remaining question is which repeated VM call or candidate enumeration causes the extra frame
returns, and how that path compares with Rakudo's actual action sequence.

Deduplicating by rule, call site and span is not a valid repair: an oracle probe with
`regex d { a || a || ab }` and an action on `d` produces `a,a,ab`, so two distinct reductions have
the same call site and span. The caller continuation also must see each path
(`t/grammar/grammar-subrule-each-path-continuation.t`). The unresolved design problem is to identify
the logical reduction event without collapsing those paths or dispatching internal matcher retries.
Keep this ADR Proposed until a focused trace explains the repeated calls and action counts and order
match Rakudo on both focused cases and the YAMLish workload.

## Semantics that the prototype must preserve

1. **Order and failed matches.** An action fires at each actual reduction,
   including a reduction later abandoned by backtracking or followed by an
   overall parse failure. Same-span alternatives and nested children retain
   their observable order. LTM probes do not create reduction events.
2. **Identity and visibility.** A capture stored under both its rule name and
   an alias dispatches once; both views keep the same identity and `.made`.
   Positional, silent, quantified, and proto captures see completed child
   results. A retained `$/` view sees the action's eventual `make` result.
3. **Environment.** `$/`, `$_`, positional and named captures, `self`, rule
   `:my` variables, and `$*` binding windows have the values visible at that
   reduction. Writes that affect subsequent matching are published once.
   Re-entrant parses and exceptions restore the caller's bindings correctly.
4. **Search behavior.** Ratchets, resumed non-ratchet callees, left recursion,
   and re-entered candidates do not accidentally suppress a distinct
   reduction or dispatch the same reduction twice.
5. **Safety.** The carrier is traced by GC, has no unsynchronized mutation
   reachable by another Raku thread, and allocates within an input-bounded,
   catchably enforced limit.

Compare these cases against Rakudo before changing the dispatch point. The
current `t/grammar` tests for alias action counts, reduce-time dynamic
variables, subrule binding windows, partial/failed parse actions, nested
captures, and retained matches provide regression coverage; add focused
cases for gaps in reduction order and retained identity. Then run the full
local gate and roast suite for the implementation.

## Implementation and measurement plan

1. Trace the repeated frame returns from the prototype by rule, caller, and
   call target, starting with the positive `<break>` lookahead. Compare a small
   action-order probe and the YAMLish action sequence with Rakudo. The wrapper
   count measurement above is not suitable for this comparison.
2. Only after that trace, identify and represent the language-level reduction
   event in the regex VM; a subrule frame return alone is insufficient (see the
   prototype result above). Use one shared dispatch path for ordinary and `$*`-dependent actions.
   Keep capture identity stable through backtracking and lazy materialization.
   Replace the replay/walk machinery only after action counts and order match
   Rakudo on focused cases and the YAMLish workload; do not leave two
   authoritative paths running actions.
3. Re-profile the 60-row case and the larger input. Report warm Callgrind Ir
   and allocator calls, then the bench CI `@section` wall-clock ratio on a
   main commit. Also measure actionless grammar parsing to catch overhead
   added to the VM's common path.

The success condition remains #7576's same-runner ratio at most 1.0 with the
same YAMLish source and correct result. This mechanism is a hypothesis, not a
claim of a 6.57x gain: if removing the post-parse walk and duplicate action
execution gives less than 2x on the YAMLish section, re-profile the remaining
cost before expanding the mechanism. A 2x result is useful evidence but is
insufficient to close #7576; the residual gap still needs a structural cause
and a measured plan. The frame-return prototype above failed the action-count
and performance checks; do not make it authoritative without identifying a
different reduction boundary and rerunning those checks.

## Alternatives considered

- **Continue per-site copy reductions.** #11948 removed 22% of warm Ir while
  leaving the large ratio unresolved. More copies may be removable, but this
  does not eliminate the tree walk, replay, or duplicate action execution.
- **Run every action only after a successful match.** This loses effects from
  failed and backtracked reductions and cannot publish `$*` writes needed by
  later matching.
- **Run actions speculatively and roll back their side effects.** Raku code can
  mutate arbitrary external state. The regex trail can undo captures and
  local bindings, not those side effects; rollback would change semantics.
