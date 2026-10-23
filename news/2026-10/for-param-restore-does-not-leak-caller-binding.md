# A callee's `for ... -> $p` no longer hands a stale `$p` to its caller

After `for @.panes -> $p { ... }` a routine put the loop parameter's prior binding back
with an ordinary env write. When that binding was only *inherited* (the program scope's
`my $p`, visible through the call chain) the write made it an own entry of the callee's
frame, so the frame merge carried it into a caller whose lexical was also called `$p`
and the caller's slot was re-pulled from it. `Terminal::UI`'s `alert` lost its pane to
the test script's file-level `my $p = Promise.new` this way. `ForParamRestore` now
records whether the saved value came from the frame's own tier and, when it did not,
lets the enclosing binding show through again (`Env::unshadow_sym`, no tombstone)
instead of copying it. `Terminal::UI` 0.1.14: all ten test files match Rakudo,
`t/07-alert.rakutest` 3/3. Fixes #14671.
