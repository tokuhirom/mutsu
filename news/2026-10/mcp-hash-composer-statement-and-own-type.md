# MCP loads and passes: statement-level hash composers, module types over caller enums

Two fixes made MCP's server, client, resource-template and coverage suites
pass:

- **A statement-level brace whose first element is a pair is a Hash even
  with a non-pair element.** `{ a => 1, ($x ?? (b => 2) !! Empty) }` was
  parsed as a hash composer, but the `hash(...)` builder it becomes when an
  element is not a pair was not recognised by the statement parser. So a
  routine ending in that brace returned a Block's List. MCP's `initialize`
  response is written this way.
- **A module routine's bare type name means its own package's type.** A
  routine runs in its caller's env, so a bare enum member the caller imported
  (MCP::Types' `LogLevel::Error`) shadowed the routine's own package class
  (`MCP::JSONRPC::Error`) in `Error.from-hash(...)`. The package's own type
  now wins over an enum member declared outside its package chain.

MCP's remaining failures (HTTP/SSE transports, OAuth, stdio integration) fail
the same way under rakudo in this environment.
