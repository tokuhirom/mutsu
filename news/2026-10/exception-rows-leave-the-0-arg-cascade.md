# A built-in exception's rows leave the 0-argument cascade

`message`, `gist`, `Str`, `backtrace` and `X::AdHoc.payload` of a built-in
exception instance are answered from the method table's owner rows by the
interpreter's entry (`exception_rows_by_owner`), not by a receiver block inside
`builtins::methods_0arg::dispatch_core`. Part of #14328 (the Match-family half
stays open).
