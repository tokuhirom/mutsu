# A parameter trait on a `method` reaches the user's `trait_mod:<is>`

`method m(Int :$val! is mcp-type<number>)` never called an exported
`multi trait_mod:<is>(Parameter $p, Str :$mcp-type!)`: only sub and lambda parameters were checked
for custom traits, so the role the handler mixes into the `Parameter` never showed up in
`.signature.params` (MCP::Server's `mcp-type` overrides, #14682). A method's parameters are now checked
when the method is registered, in the declaring scope; an unknown trait is refused there as for a sub.
