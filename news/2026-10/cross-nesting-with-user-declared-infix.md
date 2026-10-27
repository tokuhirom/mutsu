# A declared infix:<X> no longer flattens a parenthesized product

With a user `infix:<X>` in scope (Red declares one), a call that the user candidates declined fell back
to a pairwise fold that extended the left operand's rows, so `(A X B) X C` came out flat. The fallback
now builds one n-ary product over the operands as written, matching rakudo (`((a, b), c)`).
