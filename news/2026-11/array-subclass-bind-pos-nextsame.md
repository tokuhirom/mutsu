# `nextsame` from a user `BIND-POS` on an `is Array` subclass binds

`class W is Array { method BIND-POS($i, \v) { nextsame } }` answered `Nil` and bound nothing:
the base candidate of the deferral chain (`native_array_storage_base`) had no `BIND-POS`. It now binds
the element in the backing storage's shared node, as the override-free `BIND-POS` does, so
`callsame`, `nextsame` and `nextwith $i, v` bind the element and answer the bound value. Array::Rounded's
`t/01-basic.rakutest` passes 35/35 (#15989).
