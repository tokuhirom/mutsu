# `.keys` and `.item` of an infinite sequence stay lazy

`(1..*).map(* + 0).keys` and `.item` threw `X::Cannot::Lazy`: both were in the table of methods
that reify a lazy receiver first. Rakudo answers them without reifying, so `.keys` is now a lazy
index pipe like `.kv` and `.pairs`, and `.item` puts the sequence in a `Scalar` as it is (a pull,
ADR-13891). The audit also merged the two copies of that table, the runtime lazy step's and the
VM's forcing lanes', into one (`method_table::lazy_pull::reifies`).
