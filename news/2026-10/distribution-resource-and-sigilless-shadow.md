# `%?RESOURCES` entries are Distribution::Resource, and closures keep an enclosing `\name` visible

Working the `Intl::CLDR` ecosystem distribution (locked on #11256) exposed two gaps.

A `%?RESOURCES{...}` entry now binds to a `Distribution::Resource` parameter, and its
`.split` splits the file content rather than the path string. Entries stay `IO::Path`
instances, carrying a hidden `resource` marker.

A closure that reads an enclosing sigilless binding (`\attr`) while also declaring
`my $attr = attr` used to read its own new local. The bare word now resolves to the outer
binding and stays captured.

`Intl::CLDR` `t/01-spot-check.rakutest` now passes the Characters and Context transforms
subtests; Dates needs #11295 (a `|c` capture loses the caller's `is rw` containers).
