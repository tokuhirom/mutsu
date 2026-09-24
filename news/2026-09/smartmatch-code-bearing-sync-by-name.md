# `~~` no longer publishes the whole frame for embedded regex code, `s///` or a junction of regexes

Before a `~~` runs its RHS it has to publish into `env` any frame local that
the regex engine reads *by name*. The engine resolves a pattern's
interpolations, the code embedded in the pattern, and a substitution's
replacement against `env`, not against the frame's slots. [#9276] already
narrowed that publish to the variables spelled in a plain regex pattern.
Three shapes still fell back to re-broadcasting every local slot, O(L) per
match with L = frame locals:

- a regex that embeds code (`{ }`, `<?{ }>`, `:my`);
- a destructive `s///` / `tr///`;
- a junction or collection RHS value.

Those three were the last `Rakudo: O(..) -- see #9169` deficits.

`src/vm/vm_smartmatch_sync.rs` now handles all three by name:

- **Embedded code** is scanned as code. Every sigiled variable counts as a
  candidate, and so does every bare word, both as itself (a sigilless
  variable, `self`) and as `&word` (a lexical routine). Publishing a name
  nobody reads is harmless, so the scan is deliberately a superset. The scan
  is now char-based, so a non-ASCII identifier (`$café`) is no longer cut at
  its first multibyte char.
- **`s///` / `S///`** contribute the names in the pattern and in the
  replacement. The replacement is a `qq` quote, and its `{ }` code is scanned
  as code. `tr///` interpolates nothing and contributes nothing.
- **A computed RHS value** is searched for the regexes it holds: a junction,
  an array or `Seq`, a slip, a pair's value, a capture, a mixin's inner value,
  a container's content, and a named regex's captured pattern. A hash counts
  only when the LHS is a hash too. `Hash ~~ Hash` matches value against value,
  while any other `~~ %h` is a key lookup and runs no regex. The match visits
  those elements anyway, so the search adds O(k), not O(L).

`scripts/vm-complexity-check.sh` gained three cases for these shapes: a
`<?{ }>` regex, `s///` with an interpolated replacement, and a junction of
regexes. All three are flat when the frame's locals double.

Two cases still publish the whole frame, because no scan of the source can
bound what they read:

- embedded code that looks a name up indirectly (`EVAL`, `::($name)`,
  `MY::`, `callframe`, ...);
- a lazy-list or `Proxy` RHS.

Those moved to [#9293] with a fix direction. #9169 closes with this change.

Pin: `t/vm/writeback/smartmatch-code-bearing-sync.t`, which reassigns an
outer lexical before each match so that a stale env entry would give a wrong
answer.

[#9276]: https://github.com/tokuhirom/mutsu/pull/9276
[#9293]: https://github.com/tokuhirom/mutsu/issues/9293
