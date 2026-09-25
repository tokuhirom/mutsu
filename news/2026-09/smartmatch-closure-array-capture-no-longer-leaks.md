# A smartmatched closure sees its own `@` capture

`sub mk(*@m) { sub ($v) { say @m } }; 5 ~~ mk(1); 5 ~~ mk(3)` printed `1`
twice (#9429). WhereList builds its `where` matchers exactly this way, so a
`subset ... where all-items ...` declared first made every later matcher check
the subset's matchers instead of its own.

`~~` against a Sub goes through `call_sub_value`'s interpreter carrier, and two
of its env merges treated arrays specially:

- On exit, every array in the callee env was written into the caller env
  ("arrays are Arc-shared; reassignment should propagate"), including the
  closure's own `@m`, which is no caller variable at all.
- On entry, a caller array of the same name won over the closure's capture.

So the first call leaked `@m` into the mainline env and every later call read
it back. Both merges now leave alone an array the closure lexically owns (the
compiler's `authoritative_free_vars`, the same set the scalar and `merge_all`
paths already honour), so the capture wins and never escapes. A same-named
caller `@m` is no longer shadowed or overwritten either.

Also fixed: `Code.ACCEPTS` fell through to method composition and answered a
`<composed-method:ACCEPTS>` Sub; it now calls the code
(`self.count ?? self($topic) !! self()`) and answers the call's result.

Pin: `t/regex/match/smartmatch-closure-array-capture.t`.
