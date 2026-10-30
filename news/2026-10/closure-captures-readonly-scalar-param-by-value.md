# A closure captures its routine's readonly scalar parameter by value

A closure created in a routine (`$!lock.protect: { ... $id ... }` inside
`method !claim($id)`) now freezes the routine's readonly scalar parameters
(`$x` without `is copy`/`is rw`/`is raw`, like the `&`-parameters before them)
into its upvalues. Before, it resolved the name at call time, and a thread
cloned while the spawner held a shared cell under the same name answered that
cell's stale value: from the second `MCP::Client::Transport::Stdio` instance of
a process on, `Correlator!claim` saw the first request's id, so every answer was
"dropped as late" and `t/08-stdio-roundtrip.rakutest` timed out.
`t/07`, `t/08` and `t/09` of MCP::Client now all pass (#15418, #15625).
