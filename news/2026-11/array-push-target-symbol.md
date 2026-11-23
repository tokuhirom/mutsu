# An array push asks for its target by symbol

`@a.push(...)` looks up its target array by name several times per call: to
check for aliasing, laziness and `List`-ness, and to dispatch, type-check and
store. Each lookup took the name as text and interned it again. The op now
takes its constant-pool name's memoized `Symbol` once and passes it to every
lookup. That removes a few interns per push, which came to about 0.5% of the
MCP::Server::Tool::Web `html-to-text` probe (#18274).
