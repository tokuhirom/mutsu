# `my $x = try { $handled-failure }` keeps the Failure

A handled `Failure` is an ordinary value: `try` answers it as the block's value
and leaves `$!` as `Nil`. mutsu's `try` already did so as an expression. But
when the `try` initialized a scalar declaration (`my $s = try { $g }`), it turned
*any* Failure into `Nil`, so `$s` read `Any` (#18604). That decay exists for the
Failure the `try` itself caught (`my $x = try %*UNDECLARED` leaves `Any`, as in
Rakudo), and now applies to that one only.
