# A `proto method` in the program no longer hangs the first call of a same-named method on a built-in class

When a program declared `proto method add(|)` (Log::Dispatch does), the first
`.add(...)` on a built-in class such as `$*TMPDIR.add("x")` (an `IO::Path`) took
the registry read guard for the proto-method lookup and then asked for the
class's MRO while still holding it. An uncached MRO is computed and stored under
a write guard, so this was a read -> write upgrade on a non-reentrant lock:
debug builds panicked and release builds hung forever (Log::Dispatch's
`t/030-file.rakutest`). The MRO is now resolved before the read guard is taken.
Log::Dispatch `t/030-file.rakutest` passes like rakudo.
