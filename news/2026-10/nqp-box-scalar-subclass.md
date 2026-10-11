# `nqp::box_s` / `box_n` box into user `Str` / `Num` subclasses

`nqp::box_s($str, $type)` returned a plain `Str` whatever `$type` was, so
`nqp::box_s("abc", ValueObjAt)` was not a `ValueObjAt` and `ValueMap.WHICH`
(which binds that into `has ValueObjAt $!WHICH`) died with a type-check
failure. `box_native_into_class` now has one rule for every `box_*` op: a type
operand that is a user class inheriting `Int`, `Num` or `Str` is allocated with
`nqp::create` and gets the value as its payload (the core `ObjAt` /
`ValueObjAt` are built around their `WHICH` string), and the matching
`unbox_*` reads it back. Boxing a value of the wrong kind dies as MoarVM does
(`This type cannot box a native string: P6opaque, MyInt`). `box_i`'s own
`is Int` branch folded into the shared rule.

`immutable` 0.0.3's `t/01-basic.rakutest` goes from 8/14 to 13/14; the
remaining test is a multi-dispatch ranking problem, filed as #12710.
