# A bare sigil in a quoted regex atom no longer defeats the regex parse cache

`body_wants_thunk` treated any `$`, `@`, `%` or `&` inside a double-quoted regex
atom as an interpolation, so `"@"` -- a literal at sign in Raku -- marked the whole
pattern as reading ambient state. Such a pattern was never memoized: every match
re-parsed it and rebuilt its longest-token-match automaton. Cro::Uri's `authority`
token (`[ <userinfo> "@" ]? <host> [ ":" <port> ]?`) is that shape, so every
`Cro::Uri.parse` / `.add` paid for it.

A sigil now counts as interpolating only when something that can start a variable
follows it (a name, a twigil, a contextualizer, a match variable). A
`Cro::Uri.parse` with actions went from 3.63M to 2.05M instructions (callgrind
slope, -43%); MCP::Server::Tool::Web resolves one URI per link (#17385, #16897).
