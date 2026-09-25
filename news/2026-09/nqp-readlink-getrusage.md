# `nqp::readlink` and `nqp::getrusage`

Both were unsupported (#9348); P5readlink and P5times call them.

- `nqp::readlink($path)` answers the symlink's target. A regular file or a
  missing path dies the way MoarVM does, with libuv's wording:
  `Failed to readlink file: invalid argument` / `... no such file or
  directory`. The reason text comes from a small shared helper,
  `libuv_style_reason`, which turns an OS error into libuv's lower-case form.
- `nqp::getrusage(@ints)` fills a native int array with the 18 rusage fields
  in MoarVM's `MVM_proc_getrusage` order (user seconds and microseconds,
  system seconds and microseconds, then maxrss ... nivcsw) and answers null,
  as MoarVM does. The single `getrusage(RUSAGE_SELF)` reading now lives in
  `builtins::process_rusage`, which the `times` builtin also uses instead of its
  own copy. On a target without `getrusage` (wasm32) the fields are 0. The
  `nqp::const::RUSAGE_*` field indices P5times reads the array with
  (`RUSAGE_UTIME_SEC` ... `RUSAGE_NIVCSW`, 0..17) are compile-time constants
  now too.

Both were checked against rakudo 2026.07. Pinned by
`t/vm/nqp-readlink-getrusage.t`.
