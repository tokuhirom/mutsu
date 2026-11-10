# A named argument inside a `sub EXPORT` hook is no export key

The parse-time scan of a module's `sub EXPORT` hook collected every literal
`key => value` pair in the hook body as a name the hook exports, including a
colonpair named argument (`M::tj($o, :pretty(True))`) in a routine the hook
declares. A module that `use`d it then bound `pretty` as an imported lexical
(`Any`), which shadowed the same-named parameter read by a nested sub of the
module's own routines whenever they ran from a method (JSON::Fast `to-json`,
`:pretty` came out compact). The scan now skips colonpair forms and the bodies
of routines declared inside the hook.
