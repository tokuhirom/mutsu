# A dying `where` clause is returned, not parked for the next op

Raku propagates an exception thrown by a `where` constraint out of the whole
dispatch. mutsu used to record it in `Interpreter::pending_where_exception`
and rely on the dispatch funnels — or, as a backstop, `exec_one` after every
single executed op — to notice and raise it.

Now every matcher returns it: `where_truthy`/`where_smartmatch`, the signature
matchers (`args_match_param_types`, `method_args_match`,
`sub_signature_matches_value`, ...) return `Result<bool, RuntimeError>`, and
method resolution returns `MethodResolved` (`Result<Option<(Symbol,
MethodDef)>, RuntimeError>`), the method-side twin of routine resolution's
`Resolved`. A caller that dispatches the call it resolved propagates the error;
a probe (`^can`, a fast lane deciding whether it applies, a `MUTSU_VM_STATS`
shadow check) drops it and leaves the real call to raise it.

The per-op probe in `exec_one` is gone, and matching stops at the first `where`
that dies instead of evaluating the remaining parameters' constraints after it.
ADR-10779 D3: `Interpreter` loses a field (84 → 83).
