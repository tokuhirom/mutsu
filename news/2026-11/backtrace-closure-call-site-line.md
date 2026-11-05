# A closure's frame is entered from the line of its call

A sub that calls a stored or immediately-invoked closure (`my $c = { die }; $c();`) showed
in `.backtrace` at the line its own caller had called it from (`in sub g ... line 5` for a
`g` on line 1), not at the line of the closure call. `CallOnValue` set the current line, but
the closure entry then re-applied a pending call-site line left by the earlier call of the
sub (`inject_pending_callsite_line`). The `CallOnValue` arm now clears that pending line,
since the call site it just synced is the authority (#16770, found by the raku-doc
differential sweep, `Type/Backtrace.rakudoc`).
