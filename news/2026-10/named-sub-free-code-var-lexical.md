# A named routine's call to a free `&code` variable uses its own binding

`my &g = {...}; sub helper { g() }` called from a scope with its own `my &g`
used to run the caller's `&g`. Named subs now resolve a free `&name` the same
way they resolve a free scalar: through the unit-lexical cell (mainline subs)
or the per-activation lexsub alias (subs nested in a routine), so `helper()`
runs the `&g` it closes over (#10483).
