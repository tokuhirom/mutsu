# Grammar reduce replay no longer filters spans quadratically

The deterministic bench series showed `bench-yaml-parse-big` jumping from
1.84G to 4.98G instructions (2.71x) on 2026-09-24 ([#9286](https://github.com/tokuhirom/mutsu/issues/9286)).
Bisection pinned it on #9271. That PR made a childless silent subrule call
(`<.space>`) keep a hidden action node, so that its action method runs the way
Rakudo runs it (Rakudo reduces every subrule call, captured or not). That
change was correct: YAMLish declares `method space($/) { make ~$/ }` and calls
`<.space>` from 44 places, and before #9271 mutsu never ran that action.

The cost did not come from running those actions. It came from what happened
to their log afterwards. Every reduce is logged so that
`replay_reduce_action_entries` can replay the actions of branches that
backtracked. To pick which entries to dispatch, it tested every entry against
every other one for span containment. Once each `<.space>` reduce was in that
log, the pairwise filter took **57% of the whole parse's instructions**.

The filter is now a sort followed by one linear sweep, in
`methods_grammar_replay_spans::maximal_span_indices`. It selects exactly the
same entries: those not contained in a larger span, and for equal spans only
the last one logged. A unit test checks the sweep against the old pairwise
definition on random spans.

A second, smaller change: a childless silent call now builds its hidden node
only when the live parse's actions class declares a method for that rule (or
for its `:sym<...>` variant). A `<.ws>` whose grammar has no `ws` action goes
back to the pre-#9271 cheap path. The marker capture key is also interned
once per `<subrule>` atom (`NamedRegexLookupSpec::silent_marker_sym`) instead
of being built with `format!` on every call.

Result on `bench-yaml-parse-big` (local `scripts/bench-det.sh`, warm):
4.99G to 2.19G instructions. The remaining ~0.35G above the pre-#9271 figure
is the cost of actually dispatching `method space` on every `<.space>`, which
the earlier numbers never paid because the action was silently skipped.
`bench-yaml-parse` goes from 404M to 387M.

Pinned by `t/grammar/grammar-silent-subrule-action-gate.t` (a silent call's
action runs once per call, and a silent call without an action method stays
invisible) and by the unit tests in `src/runtime/methods_grammar_replay_spans.rs`.
