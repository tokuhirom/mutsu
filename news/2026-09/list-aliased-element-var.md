# List elements retain aliased scalar containers

An anonymous `$` in a List now contributes its state scalar container, so
assigning through that List element updates the value. An indexed `.VAR` read
also preserves an existing scalar cell in a List: both `($value, 2)[0].VAR` and
a List stored in a scalar report `Scalar`. Plain List values remain immutable.

This resolves #9782.
