# `Hash.STORE` checks the stored values against the hash's element type

`Array.STORE` refused a value of the wrong type, but `Hash.STORE` stored it:
`my Int %h; %h.STORE((a => "str"))` and `Hash[Int].new.STORE(...)` replaced the
pairs with a `Str` value. The stored values (a `Pair`'s, a `Hash`'s, or the
odd items of a flat key/value list) are now checked against the element type
before the node is replaced, so a refused store raises
`X::TypeCheck::Assignment` and leaves the hash as it was. This is the store
AttrX::Mooish's typed hash attributes go through.
