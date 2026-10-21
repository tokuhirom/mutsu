# `Attribute.get_value` hands out the attribute's container

`$attr.get_value($obj)` answered the attribute's plain value, so `nqp::iscont`
was false and a store through the result was refused. AttrX::Mooish relies on
Rakudo's behavior (`nqp::iscont($attr-var) ?? $attr-var = $v !! $attr-var.STORE($v)`)
and died with `No such method 'STORE' for invocant of type 'Stringy'` for a typed
attribute holding a type object. A scalar attribute now answers its Scalar
container; an absent slot still answers a plain `Nil`, which the lazy
initialization idiom (`get_value // set_value`) races on.

A Proxy subclass's public accessor now answers its attribute's value instead of
the cell it is bound to, so a captured `:old-value` no longer follows the next
store (#14266).
