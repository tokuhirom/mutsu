# Any's list-derived methods route through a user-defined `list`

A user class that defines `method list` now gets `.elems`, `.Slip`, `.flat`,
`.Seq`, `.Array`, `.List` and `.hash` (and `%$obj`) from it, as in Rakudo where
they are `self.list.<method>`. Prefix `|$obj` slips the user list too. `.hash`
on a plain user class with no `list` dies with `X::Hash::Store::OddNumber`
instead of "No such method 'hash'". The routing extends the existing
`try_any_list_view_method` (previously `kv`/`pairs`/`keys`/`values`) and
`exec_make_slip_op`. Found via `Math::Matrix`'s converter tests (#10501).
