# Retire the `__mutsu_ro_index` marker

`@a[i] := 42` and `%h<k> := 42` now store a read-only element cell (`Value::bound_element`), as
`.BIND-POS` / `.BIND-KEY` already did, instead of recording a name-keyed `__mutsu_ro_index`
marker. The restriction now travels with the container, so a write through a parameter or other
alias dies like rakudo. The marker, `MetaNs::RoIndex` and every check of it are gone.
