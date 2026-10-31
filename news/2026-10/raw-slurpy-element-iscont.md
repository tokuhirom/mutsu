# `nqp::iscont` / `isrwcont` see a raw slurpy element as a container

An element of a `+@values is raw` slurpy is the caller's container. Its `.VAR`
view (a container cell) was not classified as a container by the NQP container
tests, so `nqp::isrwcont(@values[0])` answered 0 where Rakudo answers 1. This
broke `annotations`' `Buffer.STAMP(+@values is raw)` (#15678).
