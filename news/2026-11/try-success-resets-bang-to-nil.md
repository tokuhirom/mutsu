# `$!` is Nil after a try that did not die

A `try` that completed normally set `$!` to the `Any` type object, so `try foo(); say $!.message`
died with "No such method 'message' for invocant of type 'Any'" on the success path. Rakudo's
RakuAST frontend, the oracle, leaves `$!` as `Nil` (`Nil.message` is `Nil`); the legacy frontend
left `Any`, which the comment in `vm_try_catch_ops.rs` documented. Both success paths (a normal
completion and a CONTROL block that handled the signal) now reset `$!` to `Nil`. Found by the
raku-doc differential (a `Type/` probe whose `$!.message` followed a succeeded class-method call).
