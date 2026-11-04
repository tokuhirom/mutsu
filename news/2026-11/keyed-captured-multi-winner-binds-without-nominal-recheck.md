# A keyed captured-multi winner binds without repeating its nominal type checks

A call through an imported multi family (a `CArray` read `$c[$i]` runs the
6-candidate `postcircumfix:<[ ]>` family `NativeCall` exports) picks its winner
from a stage program built once per argument-type key
(`captured_multi_plan_winner`). That program already answers every nominal
parameter check from the key, but the winner's bind ran
`type_matches_value(resolved_constraint, value)` for each parameter again,
including the `CArray` / `Iterable` name resolution it starts with (about 29k
Ir of a 145k Ir read).

`call_captured_multi_winner` now marks the winner it got from a keyed program
(`DispatchState::nominal_verified_candidate`, keyed by the candidate's
`SubData::id`) when the candidate is the plain shape the program's verdict
covers (one positional parameter per argument, no named or slurpy parameter,
no pair argument, no wrap chain). `call_compiled_closure_in_unit` takes the
mark on entry, so a nested call can never see it, and arms a one-shot just
before the winner's bind (`pending_nominal_verified`), which the bind takes at
its entry and hands to `check_and_coerce_param_type`. Only the plain nominal
`type_matches_value` is skipped: the coercion, `@`/`%`, `&` and capture arms,
`where` / subset checks and the failure paths are unchanged.

Callgrind slope on a profiling build (second run, N 500 -> 1500 reads):
`$x = $c[$_ % 500]` on a `CArray[num64]` 145.0k -> **117.7k Ir (-19%)**.

Refs #16373.
