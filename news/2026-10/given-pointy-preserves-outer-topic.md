# Pointy `given` Preserves the Enclosing Topic

A pointy parameter on `given` or `with` now binds the expression value while leaving `$_` in the block set to the enclosing topic. This also keeps `when` matching against the enclosing topic, as in Raku.
