# `for ... <-> $x` over a shaped array reached through an expression writes back

An rw loop over a one-dimensional shaped array held in a scalar
(`my $q = @s; for @$q <-> $x { ... }`) copied each element, so the loop
variable never reached `@s`. The same happened for a row of a
multi-dimensional shaped array (`for @a[1] <-> $x { ... }`). Both now alias
the elements, as the loop over the named variable and rakudo do (#19322).

- The `for` element-alias plan treats a one-dimensional shaped array as
  promotable, like `promotable_array_len` does everywhere else.
- `.Slip` of a row view hands out the row's element containers, which is what
  the `for @a[i]` desugar iterates.

Still open: an rw loop over a multi-dimensional slice (`for @a[1;*] <-> $x`).
