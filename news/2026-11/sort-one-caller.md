# `.sort` has one caller, answered by the method table

The VM's name-matching `.sort` interceptor (`try_native_sort`) is gone
(ADR-11276 §9.203). Every `.sort` is now answered by the method table's
`sort` rows. The one `SortCaller` behind them runs a comparator or mapper
as compiled code and keys a container-wrapped element by its value, as the
interceptor did. The rows no longer call user code through the tree-walking
helpers.
