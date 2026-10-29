# `.values` of a role-mixed or `is Hash` hash hands out the hash's own containers

`.values` of a hash with a role mixed in (`my %h does R`) or of an `is Hash`
subclass copied every element into a fresh Scalar, so an element bound with
`BIND-KEY` (a plain value on rakudo) reported `.VAR.^name` as `Scalar`. Both now
go through the element-container producer a plain `Hash` uses: bound elements
stay plain, assigned ones are Scalars. `BIND-KEY` on an `is Hash` instance also
binds a cell, as the `Hash` row does. WriteOnceHash's "no containers returned"
assertions pass (#15409).
