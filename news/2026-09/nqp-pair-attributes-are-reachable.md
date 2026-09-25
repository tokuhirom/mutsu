# Expose Pair's key and value through NQP attributes

`nqp::getattr` and `nqp::bindattr` now reach a `Pair`'s `$!key` and `$!value`,
including updates made through aliases of the same Pair.
