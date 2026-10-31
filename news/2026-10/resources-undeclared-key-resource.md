# `%?RESOURCES` answers a Resource for a key the distribution does not declare

Rakudo's `Distribution::Resources.AT-KEY` answers a `Distribution::Resource`
for every key, path-less when the distribution declares none: it is defined,
truthy, has `.IO` of `Empty` and stringifies to `""`. mutsu answered `Any`, so
the guard idiom `return %?RESOURCES<libraries/helper> if %?RESOURCES<libraries/helper>`
took the wrong branch and `Inline::Python3` could not load (#15816). The
hash now carries a path-less resource as its missing-key default.

A `libraries/` resource shipped under exactly its declared name (a file `foo`
for `libraries/foo`) now resolves to that file instead of the platform
library name `libfoo.so`.
