# The compiled regex engine runs `:i` and `:m`

The third part of ADR-0135's Slice B (#10252) compiles case-insensitive and mark-insensitive
patterns. Neither needed a new definition of what matches.

- Under `:i`, each atom carries its own pattern level's flag and hands it to the walk's atom
  tests. A scoped `[:i …]` covers only its body, exactly as in the walk. The engine's ASCII fast
  path probes each atom under that flag. An atom whose case fold can consume more than one
  character skips the fast path.
- Under a whole-pattern `:m`, the engine runs the mark-stripped pattern over the subject's
  stripped view, then maps positions and captures back with the walk's own remapping code. That
  code now lives in `regex_ignoremark.rs`, where both engines use it.

A whole-pattern `:i` over a character whose fold expands (`ß`, `ﬁ`) matches on a case-folded copy
of the pattern. That copy used to be rebuilt on every match. It is now cached on the pattern, so
the compiled program behind it is built once.

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), compiled patterns
went from 5,679 to 5,954 and declined ones from 1,577 to 1,345. The `ignorecase` reason is gone,
and `ignoremark` fell from 22 to 18: the scoped `[:m …]` groups that remain.
