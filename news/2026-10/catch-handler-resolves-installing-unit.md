# CATCH handlers run in the installing unit, and anon-sub `return` from them lands

An inline `CATCH` handler runs at the throw site. When the throw happened in a
different compunit than the one that wrote the `try`, the handler's unqualified
calls to routines the unit had imported died with `Unknown function`, because
the executing-unit walk landed on the dying routine's frames; the frame pushed
for the handler now carries the installing unit's file. Separately, a `return`
raised from such a handler could not find an anonymous `sub` on the stack
(frames of anonymous subs never recorded their callable id), so it surfaced as
`X::ControlFlow::Return`. Both were hit by `MCP::Server`'s `tools/call`, which
made the `MCP::Server::Tool::Ask` suite's `t/02-tool.rakutest` pass.
