# Class body exit no longer persists `$/` and `$0` as class statics

`persist_class_body_statics` snapshotted every leftover env entry of a class body into
`package_lexicals`, including the per-match state `$/`, `$¢` and the positional captures.
A later read of `$/` or `$0` under that package then returned the stale snapshot, so a
regex code assertion inside a class-scoped `subset ... where` (`<?{ $/[*-1][*-1] < 256 }>`)
saw `Nil` and accepted everything. Match-state names are now skipped. Found through the
Net::Whois::Async test suite (`IP` subset); `t/01-basic` and `t/03-subsets` now pass.
