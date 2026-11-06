# A trailing comma ends a builtin listop's arguments instead of making a list

`join '', "a", "b", ;` returned `("ab",)` instead of `"ab"`: the builtin-listop
argument loop consumed the trailing comma to see that no argument followed, but
broke out without advancing past it, so the statement parser read the leftover
`,` as a one-element list around the whole call. Only builtin heads (`join`,
`uc`, `sprintf`, ...) were affected; user subs already advanced. The loop now
keeps the position after the comma.

Found by the Selkie distribution: `app-terminal-cleanup-sequence(--> Str)`
ends its `join` with a trailing comma, so every call died with a return-type
check failure. `t/70-terminal-cleanup` and `t/82-app-facade` now match raku.
