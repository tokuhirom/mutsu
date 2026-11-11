# EEL's own `&make`: caller frames from closures, `$!made`, slangs after a regex code block

EEL 0.3 exports its own `&make` (an anonymous sub) that finds the match to
write by reading its caller's `$/` with `nqp::getlexcaller`, then binds
`nqp::bindattr($slash, Match, '$!made', $value)` when it is an `NQPMatchRole`.
Its `t/44-actions-dynamic-repeat.t` went from dying at compile time to 55 of 56.

- A closure's call pushed its caller-frame entry after the callee's own frame
  overlay existed, so `CALLER::` / `nqp::getlexcaller` from an anonymous sub saw
  the callee's fresh `$/` instead of the caller's; the entry is now pushed
  before, as a named routine's is. `nqp::getlexcaller` marks a chunk as reading
  its caller's frame, and such a routine no longer takes the TRIR call path,
  which pushes no frame.
- `nqp::bindattr(..., Match, '$!made', ...)` is `make`; a `Match` does
  `NQPMatchRole`, which is a known type name; `nqp::ctxcallerskipthunks` and
  `Rakudo::Internals.IS-ISOLATED-MATCH` exist.
- Parsing a regex code block (`/ a { 17 } /`) ran a nested parse that reset the
  unit's slang hooks, parser modes and declarator keywords, so a slang's
  `pattern` declarator after it no longer parsed. The nested parse now
  restores them.
- A slang declarator (`event`) used in a module unit that `use`s the slang is
  built with the slang's metaclass (see `slang-declarator-in-module-unit.md`).
