# `CompUnit::Handle.export-package` and `.export-sub` for a package-less module

A module file without a `unit module` declaration now lists all its export
tags in `$handle.export-package` (`ALL DEFAULT rob-schnider`, not just `ALL`),
`$handle.export-sub` answers the module's `sub EXPORT` after the load instead
of `Nil`, and `%h.append(Stash)` adds the stash's symbols the way it does for
any `Map`. CompUnit::Util's `t/04-capture-import.rakutest` goes from 1 to 17
passing tests. (#15371)
