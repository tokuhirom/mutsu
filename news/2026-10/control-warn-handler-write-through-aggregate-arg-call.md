# A resumed CONTROL handler's write survives a call that passes an aggregate

A `CONTROL { when CX::Warn { $m = .message; .resume } }` handler is run inline at
the `warn` site. Its write to the installing closure's lexical was carried back
up only by name-based drains, which a callee that received an aggregate variable
(`warner %d`) restored over on return, so `$m` stayed empty. The handler's
writes are now recorded as by-name writes that cross every frame exit
(`record_runtime_name_write`), matching how `$::($n) = v` is carried.

Found while working the JSON::Unmarshal ecosystem suite
(`t/010-basic.rakutest`, "unsupported named arguments result in a warning").
