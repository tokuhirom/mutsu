# Assigning through an `@`/`%` accessor calls a bound Proxy's STORE

`$obj.attr = v` on an `is rw` attribute that holds a `Proxy` already called the
Proxy's `STORE` for a `$` attribute; for an `@` or `%` attribute it replaced the
slot and dropped the Proxy. AttrX::Mooish binds an `AttrProxy` into array and hash
attributes, so a trigger on one never fired. The accessor assignment now goes
through the Proxy for every sigil. AttrX::Mooish's `t/012-hashes` passes in full
(#14266).
