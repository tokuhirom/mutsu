# Crane's `in` and `patch` test files pass

Three fixes let the Crane distribution's whole suite pass:

- A callee's sigilless parameter no longer leaks its alias entry back into a
  caller whose own sigilless parameter has the same name. The return-side env
  merge copied `__mutsu_sigilless_alias::c` over the caller's, so the caller's
  writeback on return overwrote the inner call's argument variable
  (`sub top(\c) { my $root = c.clone; leaf($root); $root }` returned the
  unmodified clone).
- Assigning to a type-object `is rw` method call that carries arguments now
  raises the method's own exception. The failure was swallowed and the method
  re-called with only the assigned value, so `Crane.in(%h, @bad-path) = $v`
  silently succeeded instead of throwing `X::Crane::PositionalIndexInvalid`.
- A `WhateverCode` position into a container that does not exist yet
  (`return-rw c[*-0]`, `c[*-0] = $v` with `c` bound to a missing hash entry)
  counts from the end of the empty Array the write creates, instead of being
  stringified into a hash key.
