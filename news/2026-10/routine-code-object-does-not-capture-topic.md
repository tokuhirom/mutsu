# A routine's code object no longer captures the declaring frame's topic

A nested closure reading `$_` makes `_` a free name of the routine around it,
and `routine_code_object_env` then captured the topic of the frame that took
the routine's code object. For a `sub EXPORT` hook holding a lexical sub that
frame had a topic, so the exported routine pinned it: `@a.map({ to-json($_) })`
with `JSON::Fast` answered `"a","a","a"`, and `JSON::Fast::Hyper`'s `to-json`
of an array repeated the first element (#14500). A routine's `$_` is its own,
so the code object never captures it.
