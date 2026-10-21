# `$!attr := Proxy.new(...)` seats the Proxy as the attribute

Binding an attribute to a `Proxy` inside a method dropped the Proxy: the slot was
treated as non-mirrorable, so `$!attr` kept reading the old value and the
`is rw` accessor assigned over it instead of calling `STORE`. A `Proxy` bound to
an attribute is now seated in the instance like any other bound container, so
reads FETCH and writes STORE, inside the method and through the accessor
(#14266, AttrX::Mooish root cause 1).
