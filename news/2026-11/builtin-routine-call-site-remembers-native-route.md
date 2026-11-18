# A built-in routine call site remembers its native route

A call such as `abs($x)`, `max($a, $b)`, `chr($n)` or `uniname($_)` names a
routine no user code declares, yet every execution walked the whole function
resolution ladder — the light and OTF caches, the lexical `&name` override,
CALL-ME, autothreading, the export hook, the wrap chain, then the
compiled-function, proto, multi and user-shadow probes — before it reached the
builtin. That was about 15k instructions and 20 allocations per call, against
about 2k for the builtin's own work (#18028).

The `CallFunc` site now remembers, for one registry write generation, that the
native arm answered it (`CompiledCode::native_call_sites`, the same storage the
method-table site lane uses). A declaration of the name moves the generation;
everything bound at run time — a lexical or imported `&name`, a wrap, an
EXPORT-hook override, a `my method` of the name, an lvalue context, a Junction
or Proxy argument — is re-checked on every hit and sends the call down the
general path. When the builtin declines a particular set of arguments, the call
continues at the general path's dispatch, so the memo only ever decides how
much work a call does, never which routine answers it.

`abs($b)` in a loop went from 15.1k to 6.4k instructions per call (callgrind
slope on a profiling build), and from ~2.2 µs to ~0.6 µs.

Working this turned up two older bugs, filed separately: a builtin called with
a Junction argument does not autothread (#18098), and a Proxy passed through a
plain sub parameter reaches the builtin unFETCHed (#18099).
