# The candidates of a re-exported infix family are ranked, not tried in stash order

A module that re-exports another module's stash (`sub EXPORT { Other::EXPORT::ALL:: }`, as
Red does with `Red::Operators::EXPORT::ALL::`) materializes `&infix:<op>` as a captured
candidate family. `try_user_candidates` called the first candidate that bound, in the stash's
order, so for a DateTime operand Red's `infix:<==>` picked `Date() $b is rw` (and compared the
column against `'2024-02-29'`) instead of the narrower `DateTime() $b is rw`. The family now goes
through `try_captured_candidates`, which ranks by narrowness and lets the core operator's
candidates compete, as the block-scoped and registry paths already did. Red `t/80-date-operator`
now passes.
