# `CArray.tail(n)` and `CArray.reverse` read the array's own elements

`$carray.tail(6)` answered a one-element list holding the CArray itself, so
`$module.index.tail(6).head.name` died with `No such method 'name' for invocant of type
'NativeCall::Types::CArray'`, and `$carray.reverse` died with `CArray cannot be used without
a type`. `Any.tail(n)` and `Any.reverse` are `self.list.tail(n)` / `self.list.reverse`.
The pure `tail(n)` row now declines an instance (or mixin) that does not decompose, and the
interpreter's `tail` / `reverse` rows route such a receiver through its own `list`.
`CSS::Module` 0.7.8: `t/module-extensions.t` 11/11 like Rakudo; every one of its 16 test
files now matches. Fixes part of #14099.
