# The `nqp::` exception-handling ops

All fifteen of NQP's exception-handling ops now run (#11497, part of the
`nqp::` coverage campaign #11488): `die`, `die_s`, `newexception`,
`setmessage`, `setpayload`, `setextype`, `getextype`, `throw`, `rethrow`,
`resume`, `exception` and `backtracestrings` join the existing `getmessage`,
`getpayload` and `backtrace`.

MoarVM's VM-level exception object is modelled as a `BOOTException` instance
with a message, a payload and a category. Raising one goes through the routines
the Raku spellings use, so handlers cannot tell them apart. `nqp::die` is
`die`. `nqp::throw` of a Raku exception payload throws that exception. A
`CONTROL_NEXT` / `LAST` / `REDO` / `TAKE` / `WARN` / `SUCCEED` / `PROCEED`
category raises the matching control signal, and the `nqp::const::CONTROL_*`
constants now fold. `nqp::exception()` reads a new handled-exception stack on
`ControlState`. Every CATCH and CONTROL handler run pushes onto it, whether the
handler runs while unwinding or inline at the throw or warn site. So a routine
called from a handler sees the exception too, as on MoarVM. Labeled loop
controls and `CONTROL_RETURN` stay loud errors.

While testing `backtracestrings`, this work found an older bug. The call form
of `die` (`my $x = die "x"`) reached a CATCH handler with no `Backtrace`. The
handler ran inline before the dispatch loop attached one. `die` now captures
its backtrace at the throw site, as the `Die` opcode does.
