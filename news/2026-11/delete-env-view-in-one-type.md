# `:delete`'s temporary env view lives in one type

`exec_delete_index_named_op` exposed a variable's container in `env[name]` for the
duration of the op (a scalar-held aggregate seeded from its slot, a compunit's
file-scope `@`/`%`, an `our` variable, a `:=`-bound cell, a for-bound Scalar
wrapper, a `Mixin`, a lazy array) and handed it back afterwards, in ~170 lines
of inline code spread across the handler. That is now `DeleteHome`
(`src/vm/vm_var_delete_home.rs`): the handler is "enter, delete, exit", and the
code is moved verbatim, so behaviour is unchanged (#14701, ADR-14632 follow-up).

The hole bookkeeping the issue's earlier notes listed as the next step had
already moved off the name-keyed env side table onto the array data
(`ArrayData::initialized`, `LazyList::deleted_indices`; ADR-0097), so what is
left of the issue is the inner handler's by-name reads of the container: with
the entry/exit in one place, the next slice (resolve the container once, pass it
to the inner handler, then drop the scalar seed for a `Slot` place) only has to
change `DeleteHome` and the inner handler's reads.
