# `cmp` on lists compares scalar-container elements by value

`($a, 1) cmp ($b, 1)` and a method returning `($!attr, ...)` compared their elements as
strings (`9` above `13`) whenever an element sat in a scalar container. The list
comparison now looks through the container. RakuPM's `RakuPM::Version` ordering
(`t/version.t`, `t/resolve-provides.t`) matches Rakudo again (#14170).
