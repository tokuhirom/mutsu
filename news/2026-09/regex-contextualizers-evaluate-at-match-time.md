# Regex `$(...)` / `@(...)` are evaluated at match time, on the caller

The last scratch `Interpreter` on the regex side is gone (#10157). The
interpolation pre-pass (`interpolate_regex_scalars`) used to evaluate a
`$( code )` / `@( code )` contextualizer — and a `"…$x.meth()…"` chain in a
double-quoted atom that no compiled qq thunk covers — while it built the
pattern text. The regex parser only holds `&self`, so
`eval_string_as_source` built a whole new interpreter (a full
`Interpreter::new()`, an env clone and a registry copy) for every evaluation.

The pre-pass now leaves the code in the text (rewriting the double-quoted
chain to `$( $x.meth() )`), and the structural parser lowers it to a new
`RegexAtom::CodeInterp` atom. The matcher, which holds `&mut self`, runs the
code when the cursor reaches the atom, through `run_regex_sub_eval` over the
same env a `<{ … }>` interpolation gets (`regex_code_interp_env`, now shared by
both): the scalar form matches its value literally, the list form an
alternation over the elements, with every candidate end exposed to
backtracking. That is also when Rakudo evaluates the atom, and the text the
pre-pass produces no longer changes from one evaluation to the next, so the
interpolated-pattern parse cache now hits for these patterns.

The atom is opaque to the static analyses, like `VarInterp`, and ends the
declarative LTM prefix (ADR-0046 probe Q). `eval_string_as_source`,
`copy_decl_registry_into`, `copy_full_registry_into` and
`interp_closure_scope_snapshot` are deleted, and
`scripts/interp-construction-allowlist.txt` loses its last regex row.
