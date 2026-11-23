# A parenthesized zero-argument call skips the general call path

A parenthesized call of a routine with no parameter (`sub e0() { }; e0()`) went
through the general call path on every call. That path runs the multi-candidate
probe, the `&name` override lookup, the argument decoding and the rest. A
one-argument call of a typed routine (`e1('a')`) was already served from the
name-keyed call cache, and was cheaper (#18080).

There were three causes:

- The parser's callsite-line marker, which every `f()` carries so that test
  assertions and deprecation reports can name the line, was compiled as a
  named argument. That made the call a `CallFuncNamed` and shut every
  positional door. It now stays an in-band constant, and the argument
  descriptor no longer counts it as named.
- The cache's TRIR door refused any call carrying the marker. It now records
  the marker's line and pops the marker, as the light path does.
- A routine with no parameter never entered the cache. The general path's
  resolution-cache hit now installs it. The cache's hit path serves it with
  TRIR when the routine has a chunk, and otherwise through the same
  `call_compiled_function_fast` as before (`vm_call_zero_param.rs`).

Routing these calls into TRIR exposed two gaps in TRIR, both now fixed:

- A `die` inside a TRIR body reported the line of whatever called the routine,
  because TRIR never updated the current source line. Each TRIR call site now
  records the line of its statement and adopts it before calling out. This
  also fixes the backtrace line of typed routines, which had been wrong on
  every call.
- TRIR bodies count no lines for the profiler, so every TRIR door now declines
  while the profiler is armed.
