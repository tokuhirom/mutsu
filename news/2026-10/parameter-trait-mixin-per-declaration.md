# A parameter trait's mixin keeps its value, per declared parameter

Rakudo builds one `Parameter` object per declared parameter. A
`trait_mod:<is>(Parameter $p, ...)` candidate mutates that object, and every
later `.signature.params` hands out the same one. MCP::Server relies on this:
its `is mcp-type<number>` does `$param does HasMCPType($mcp-type)` and reads
the type back from the signature.

mutsu built a fresh `Parameter` on every `.signature.params`. It only
remembered which mixin type each trait name had produced, keyed by the name
alone. So the role's attribute value was lost, and `is mcp-type<number>` and
`is mcp-type<string>` could not be told apart.

The trait now runs on an object kept on the declaration itself (`ParamDef`,
and every `SigParam` converted from it share one slot). `.signature.params`
returns that object, so `&f.signature.params[0] === &f.signature.params[0]` as
in Rakudo. The process-wide per-trait-name table is gone.

Two smaller fixes made this work:

- `$obj does R(value)` on an object now reblesses it in place and sets R's one
  public attribute. Before, it fell back to the copying wrapper, which only
  the variable that ran the `does` saw.
- A mixin type that names a lexical role displays the whole name.
  `Box+{Local}` used to print as `Box+{Local`.
