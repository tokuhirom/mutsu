# A TRIR routine's call reaches the lexical sub that shadows a core routine

A `my sub parse-base(...)` shadows the core `parse-base` for every call in its
scope. A call made from a routine the typed IR admits (`CallGen`) went through
`call_function`, the built-in funnel, whose arms answer a built-in name before
any user routine is consulted — so the TRIR body reached the core routine
instead. The VM's own call op resolves the declared routine first; `CallGen`
now does the same for a name that both a built-in and a user routine carry.

This had turned MCP::Server::Tool::Web's `t/12-extract.rakutest` red: once a
`--> Str` return type stopped keeping routines out of TRIR,
`resolve-url(... --> Str) { resolve-ref($href, parse-base($base)) }` became
eligible and died with "Cannot convert string to number" in the core
`parse-base`. The file is 13/13 again, like raku.
