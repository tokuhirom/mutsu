# The thunks a user scheduler is cued with share one compiled body

`start`, `Promise.in`/`.at` and the awaiter wake-ups hand a user `$*SCHEDULER`
a zero-argument block (`synthesized_thunk`). Each one carried its own AST body
with the `Promise::Vow` and the argument baked in as literals, so every run went
through the tree-walk carrier of `call_sub_value`. The thunks now share one
compiled body per method (`synthesized_closure_code`); the vow and the argument
ride in the captured env, vouched as authoritative so a thunk running inside
another thunk keeps its own. A slice of #14572.
