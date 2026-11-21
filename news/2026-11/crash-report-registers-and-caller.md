# Crash reports name the caller of a jump to a bad address

The one CI SIGSEGV of #16811 (IO::Socket::Async::SSL's `t/thread-stress.rakutest`)
had a pool thread inside `ffi_call` jump to a heap address, and its crash report
could not say who made that call: the faulting `pc` was the heap address itself,
which no unwinder can start from, and OpenSSL and libc keep no frame pointers.
More than 330 local release runs of the file, serial, under three-way parallel
load and under valgrind, have not reproduced it, so the next CI hit has to carry
the answer.

The fatal-signal handler now also writes the thread's registers from the signal
`ucontext` (`rip`/`rsp`/argument registers on x86-64; `pc`/`sp`/`lr` on
aarch64), the eight stack words at `sp` read with `process_vm_readv` (a bad
`sp` is an error, not a second fault), and the `/proc/self/maps` lines those
addresses fall in. After a `call` through a bad pointer the caller's return
address is `[sp+0]` (or `lr`), and its mapping turns it into "library + offset"
for `addr2line`. All of it is async-signal-safe. A new selftest,
`MUTSU_CRASH_SELFTEST=jump`, calls into a non-executable heap buffer and the
integration test checks that the report lists the caller's executable mapping.
