# The EVAL pre-run no longer stops at `use lib` for a unit that uses Test

With #16695 fixed (a hook module's exported `&trait_mod:<is>` dispatcher keeps its
candidate next to Test's), the workaround in `run_eval_hook_uses` is gone: an `EVAL`'d
unit that `use`s Test now pre-runs the `sub EXPORT` hook modules after a literal
`use lib` too, so their names are known at CHECK time (#13441).
