# At-sigil loop parameters rebind after start blocks

Named `for` parameters now stay off the cross-thread bare-name store for the
duration of the loop, including single `@`- and `%`-sigil parameters. Previously,
once a `start` block activated that store, a loop such as
`for @matrix -> @row { @row.push(9) }` could pull the previous iteration's stale
container back over the freshly bound row.

The scoped mask already used for multi-parameter loops now covers every named
loop parameter and is removed on every exit path. A regression test exercises
two independent at-sigil loops after `start` and confirms that lexical capture
of a single scalar loop parameter still works.
