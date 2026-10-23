# An `EVAL` in one routine no longer makes every call write its parameters to the env

The process-global "something may read lexicals by name" latch was set by any
compiled `EVAL`, `EVALFILE` or `.EVAL`, so a single `EVAL` anywhere in the
program (a never-called `sub ev($c) { EVAL $c }`, or a loaded module such as
`JSON::Fast`) made every argument-taking call mirror its parameters into the
shared env (#14337). Parameter binding now consults a second latch
(`NON_EVAL_REFLECTIVE_SEEN`) set only by the ops that can read *another*
frame's lexical (`CALLER::`, `OUTER::`, symbolic dereference, pseudo-stashes,
`nqp::getlex`), plus the routine's own chunk flag
(`CompiledCode::mirrors_all_params`), which covers an `EVAL` in the routine or
in a closure nested in it. Plain `my` stores keep the broader latch: an `EVAL`
in a nested sub reads enclosing chunks' locals, which no flag of theirs
records.

On the repro (1000 calls of `f($s)` next to a never-called `EVAL`)
`env_deep_copies` goes from 1000 to 0, and `use JSON::Fast` from 1008 to 8.
`use Test` still pays the write: `Test.rakumod` reads the caller's context with
`CALLER::` / `CALLER::LEXICAL::`, a genuine cross-frame reader; see the status
comment on #14337.
