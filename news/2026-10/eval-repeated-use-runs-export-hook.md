# A repeated `use` of a hook module in an EVAL runs the hook each time

`EVAL 'use M; use M'` pre-runs both `use`s ahead of the unit; the second one replayed the map the
first had recorded instead of running the module's `sub EXPORT` again. A pre-run now always runs
the hook, as rakudo does at BEGIN time (twice for two `use`s); only the in-position run replays
(#15698).
