# An EVAL'd unit's `sub EXPORT` hook imports bind in lexical import sets

The pre-run of an `EVAL`'d unit's `use` of a hook module (`run_eval_hook_uses`) now fills the lexical
import set the `use` has in the unit: a top-level one replays the recorded map in position instead of
being dropped from the unit, and a `use` nested in a block fills the very set id its block's own
`use` reads (`Lifter` computes it). A hook's routines and its non-type values under capitalised keys
(`'Thing' => 42`) therefore bind in the set, with no env alias, and the CHECK-time name checks and the
`MY::` listing read them from the sets. A routine a hook hands out to a block of an `EVAL` is no longer
callable after the block (#14140, ADR-8751).
