# `$e.throw` on a variable runs the CATCH before the LEAVE

`$e.throw` on a named receiver now handles a CATCH inline at the
throw site (ADR-0072), as `die`, `X.new.throw` and `($e).throw` already did. The
variable form took the unwinding path, so a routine's LEAVE ran before its own
CATCH. Red's `run-transaction` (`CATCH { rollback-and-rethrow }` plus `LEAVE
$guard.release`) released and rolled back the guard before the handler looked,
so a failing rollback was not reported as `X::Red::Transaction::Cleanup`
(t/99-transaction-guard).
