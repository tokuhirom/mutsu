# Assigning an aggregate to the topic `$_` itemizes it

The topic `$_` was exempt from scalar-store itemization by name, so
`for @a { $_ = [1,2] }` stored a bare Array into `@a[0]` (`[1, 2]` where
Rakudo has `$[1, 2]`), and likewise for Hashes, `given $t` and `map`. The
exemption only exists for a topic bound to a whole `@`/`%` container
(`given @a { .=reverse }`), whose write-back is that container's `STORE`.
The rule is now per holder: a topic currently holding an un-itemized
Array/Hash keeps the exemption; any other topic is a Scalar and itemizes like
every `$` store (#11229).
