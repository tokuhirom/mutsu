# `Array.Seq` now preserves element containers

`@a.Seq` was using the native structural `Seq` coercion, which copied the
array's current values before ADR-0045's container-aware producer layer could
run. Consequently, an aliasing `for` parameter bound to a value copy:

```raku
my @a = 1, 2, 3;
for @a.Seq { $_++ }
say @a; # formerly [1 2 3], now [2 3 4]
```

The native `.Seq` dispatch now first routes a real mutable plain `Array` through
`vm_element_producers.rs`, where the resulting `Seq` carries the source
elements' `Scalar` containers. The ordinary structural coercion remains the
fallback for every other receiver. `.List` is unchanged: it intentionally
decontainerizes Array elements and therefore stays immutable when iterated.

`t/for-loop-element-alias.t` covers topic mutation, an escaping `is rw` alias,
and normal value rendering of the result.
