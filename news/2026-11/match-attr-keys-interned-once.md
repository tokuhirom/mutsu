# Materializing a grammar Match no longer re-interns its attribute keys

A grammar parse with actions materializes every Match node that the action
walk visits. `MatchNode::materialize_map` built each node's attribute map
with string keys (`str`, `from`, `to`, `list`, `named`, `orig`, ...), and
every insert re-interned its literal through the thread-local symbol table.
The action walk then read `named`/`list`/`silent_caps`/`sym_variant` back
the same way. These keys are now well-known symbols that are resolved once
per process.

On `Cro::Uri.parse-ref` (the hot path of MCP::Server::Tool::Web's link
resolver), 600 parses went from 1,268M to 1,251M instructions under
callgrind, about 29k instructions (~1.7%) per parse. That is one slice of
#17385, whose remaining cost is the general per-call floor.
