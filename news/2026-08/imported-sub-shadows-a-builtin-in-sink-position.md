# An imported sub shadows a builtin in sink position too

An imported routine shadows a same-named builtin at every call site. mutsu
honoured that on the `CallFunc` path — `dispatch_func_call_inner` has had a
`user_function_matches_call` branch for it since the builtin-shadow work — but
not on `ExecCall`, the opcode a call in **sink (non-final) statement position**
compiles to. `ExecCall`'s fallback goes through `exec_call_values`, which tries
`call_function` *first* and only reaches user dispatch when the name is not a
builtin at all. So a shadowed name in sink position reached the builtin.

The symptom was position-dependent in a way that made no sense until it was
measured. `Cro::HTTP::Router` exports `get`; mutsu's builtin `get` reads a line
from a handle. Inside a `subtest`:

| route body                          | result |
|-------------------------------------|--------|
| `get -> {…}` then `note "…"`        | died with `Expected IO::Handle` |
| `note "…"` then `get -> {…}`        | worked |
| `get -> {…}` then `get -> 'y' {…}`  | died |

`get` last (so, `CallFunc`) worked; `get` anywhere else (so, `ExecCall`) did
not. `ExecCall` now checks `user_function_matches_call` before the fallback and
routes a shadowed name straight to user dispatch.

This was found by following the `# subtest died: Expected IO::Handle` line that
`news/2026-08/subtest-reports-why-its-body-died.md` added the same day, then
breaking on the error site in `rust-gdb` — the backtrace named
`builtin_get ← call_function ← exec_call_values ← exec_exec_call_op` directly.

The vendored Cro suite's `http-middleware.rakutest` subtest 6 ("Interaction of
middleware written as Cro::Transform with HTTP router") goes from a silent
`1..0` to all 11 assertions passing, leaving one failing subtest in that file
(down from four at the start of the day).

`t/imported-sub-shadows-builtin-in-sink-position.t` characterises the rule.
It passes on the parent commit too: no synthetic arrangement found so far
pushes a shadowed call onto the `ExecCall` path — the reproducers that need the
fix (`tmp/st6q.p6` and the Cro suite) both need the Cro tree.
