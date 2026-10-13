# A `:m` rule runs as a frame over a subject with combining marks

A whole-`:ignoremark` rule called as `<r>` used to run as a lazy frame only when the subject had no
combining marks. Over a subject with marks it fell back to the eager path, which computes every end
of the callee before the caller enters any of them. So a `{ … }` block inside such a rule ran once
per end computed rather than once per end entered: `regex r { :m a+ { @log.push: $/.to } }` over
`"a\x[301]aax"` logged `3,2,1` where rakudo logs `3`.

The callee is now compiled a second way, as a *markless* program, alongside the plain one. Each
consuming atom tests the mark-stripped base of the grapheme it stands on and consumes the whole
grapheme. Because the frame keeps reading the caller's own subject, positions, captures and
backtracking need no position map. A body that reads the subject some other way still takes the
eager path: a look-around, a backreference, an LTM `|`, `<.ws>`, an interpolation, an assertion,
or a nested `:m` group. Its result is unchanged, as before (#7548).
