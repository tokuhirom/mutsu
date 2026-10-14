# A rule holding `@( ... )` / `$( ... )` is a frame like any other

The left-call graph treated a code interpolation as a call it could not name, so
a rule containing one was never proven free of left re-entry and ran on the
eager growing-seed path: a code block in it ran at every end computed
(`regex r { @(<a aa>) { @log.push: $/.to } }` logged `2 1` where Rakudo logs
`2`). It is now handled as `$x` is, so such a rule is entered as a frame and its
ends are taken lazily (#13461).
