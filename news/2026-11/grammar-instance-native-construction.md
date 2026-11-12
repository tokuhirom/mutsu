# A grammar instance is built by the native default constructor

`G.new` on a grammar -- and the start-rule invocant every `G.parse` builds
with it -- always took the general `dispatch_new` path, because the native
default constructor's eligibility check refused any MRO entry that is not a
registered class: a grammar's built-in ancestors `Grammar`, `Match` and
`Capture` are not. They declare nothing the default constructor builds, so a
grammar instance is now built natively like any other class instance, with
the same attribute defaults, named arguments, BUILD and TWEAK; a grammar with
its own `new` still runs it. (A `class M is Match` is not a grammar and keeps
its Match-shaped construction.)

`G.new` drops from ~97k to ~19k instructions, and a minimal `G.parse` from
~165k to ~126k. Every `Cro::Uri` parse is a grammar parse, which is where
MCP::Server::Tool::Web's link resolution spends its time (#17385).
