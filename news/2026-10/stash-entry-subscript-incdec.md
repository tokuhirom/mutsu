# `++`/`--` through a package stash entry steps the entry

`Foo::<%reg><x>++` left `our %reg` untouched: the root-binding that single-evaluates a
non-variable chain root bound the stash itself, which is no container to write
through. A chain rooted in a package stash now binds the stash *entry*
(`Foo::<%reg>`, the shared `our` container) and continues below it, as the plain
assignment form already did (#15551). Prefix, postfix, deeper chains and `OUR::<%g>`
all step the entry.
