# The I/O, filesystem, process and system `nqp::` ops

All 42 ops of #11501 (part of the `nqp::` coverage campaign #11488) are
implemented, completing the Input/Output, File / Directory / Network,
Processes, System Introspection and Timish categories: `print`/`say`, the
`*fh` handle ops (`writefh`, `seekfh`, `tellfh`, `eoffh`, `flushfh`,
`filenofh`, `getport`), the path ops (`mkdir`, `rmdir`, `unlink`, `rename`,
`copy`, `link`, `symlink`, `chmod`, `chown`, `chdir`, `cwd`,
`fileexecutable`, `filewritable`, `stat_time`, `lstat_time`), `getpid`,
`getppid`, `execname`, `exit`, `cpucores`, `freemem`, `totalmem`, `uname`
(with the `UNAME_*` constants), `getsignals`, `getenvhash`, `backendconfig`,
`decodelocaltime` and `sleep`.

Each op shares its routine with the Raku-level API Rakudo builds on it, and
answers and fails as MoarVM does (checked against `raku`):

- The filesystem syscalls live once in `native_io::fs_syscalls`, each failing
  with libuv's `Failed to <op>: <reason>` text. `IO::Path.rmdir`, `.chmod`,
  `.unlink`, `rename` and `mkdir` now call them too.
- `mkdir` and `IO::Path.mkdir` now honour their `$mode` argument, which mutsu
  used to ignore (always `0o777`).
- `IO::Path.created`/`.accessed`/`.modified`/`.changed` read their time through
  the same `nqp_stat::stat_time` as `nqp::stat_time`.
- One signal table (`runtime::signal_table`, MoarVM's list) now backs
  `nqp::getsignals`, the `Signal` enum, `Kernel.signals` and `Kernel.signal`,
  replacing three hand-written copies.
- `Kernel.free-memory` and `Kernel.total-memory` are new, sharing
  `sys_resources` with `nqp::freemem`/`nqp::totalmem` (and `cpu-cores` with
  `nqp::cpucores`).
- `nqp::exit` ends the process like MoarVM's op: it neither calls `&*EXIT` nor
  runs END phasers, unlike Raku's `exit`.
- `nqp::backendconfig` answers the hash `$*VM.config` does: mutsu's own build
  facts, not MoarVM's.
