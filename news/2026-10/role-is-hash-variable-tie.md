# `role R is Hash` works as a variable trait, and `$_` aliases Proxy elements

Found by running the `Hash::MutableKeys` distribution's own test suite.

- `my %h is R = ...` and `my %h is R["arg"]` now work when `R` is a role that `is Hash`:
  the pun reaches `Hash`/`Map` through its MRO, so the instance gets the associative
  backing store and the `STORE`/`push`/`DELETE-KEY`/subscript delegation. A parameterised
  trait (`is R["append"]`) is evaluated to its role application first.
- A role method (`method keys`, `method elems`) on such a tie now wins over the native Hash
  method, and `callsame` inside it reaches the storage.
- `self."$name"(...)` inside a method compiles as a call on `self` with writeback, like the
  literal-name form, so a mutator reached by a runtime name updates a `Hash`/`Array` subclass.
- `for` binds the implicit topic `$_` to a `Proxy` element itself instead of its FETCHed value,
  and no longer marks it read-only, so `$_ = "x" for @proxies` and `$_ .= uc for %tied.keys`
  call each STORE.

`Hash::MutableKeys` `t/01-basic.rakutest` now passes 2 of 3; the third needs #12162.
