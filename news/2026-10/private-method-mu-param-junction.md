# A private method call binds a Junction to a `Mu` parameter whole

`self!m($junction)` was run once per eigenstate even when `m`'s parameter is
`Mu`, because the autothread check looked the method up by its `!`-prefixed
name in the public method table and, finding nothing, threaded every Junction
argument. It now resolves private methods through the private lookup, so the
parameter rules apply as they do for a public call (#15842).
