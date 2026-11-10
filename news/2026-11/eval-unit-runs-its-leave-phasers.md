# An EVAL'd unit runs its LEAVE / KEEP / UNDO phasers

`EVAL 'LEAVE { say "leave" }; say "body"'` printed only `body` when the EVAL was
called from the mainline: the unit's exit phasers were wired up only when the
compiled code was a routine body, so a unit of its own never ran them (inside a
`sub` they did). An EVAL'd unit is a scope whose exit phasers run when it ends,
wherever it was called from, so the unit now takes the phaser-scope path too.
A unit whose only exit phasers are the replays a unit-level variable trait queues
keeps the old path (its tail value is lost on the phaser-scope path after a
`trait_mod:<does>` mixin; recorded as a `// TODO` at `Compiler::has_exit_phasers`).
