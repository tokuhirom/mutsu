# Object-hash `++` keeps the key object on a captured hash

`%h{1}++` on an object hash (`my %h{Any}`) keyed the element by `Str` whenever the
hash lived in a shared capture cell, for instance after a sibling block's closure
captured a same-named `%h`. The cell writeback of `exec_inc_dec_index_op` skipped
the `original_keys` bookkeeping the uncelled writeback does; it now records the key
object too, so `.keys` answers `(Int)` as Rakudo does (#11022).
