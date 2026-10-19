# An enum declared in a block keeps its keys after the block

An enum is `our`-scoped. Rakudo installs its keys in the declaring package as
well as in the lexical scope, so after `{ enum E1 <ea eb> }` a bare `eb` still
resolves, and so does `::('eb')` (#14019). mutsu kept only the lexical half:
since #11688 a key does not outlive its block or routine, so `eb` was an
unknown bareword after the block, read as a string.

`register_enum` now also records an `our` enum's keys as symbols of the
declaring package (`record_package_enum_key`). They go in the same
package-keyed table that holds a loaded module's top-level enum keys, and
`enum_bare_value` falls back to it after the env. A lexical of the same name
still wins, which #11412 pins (an outer `Normal` over a sub's enum key), and a
key the package already holds keeps its value. A `my enum` stays lexical.
