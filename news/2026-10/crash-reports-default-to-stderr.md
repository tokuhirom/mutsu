# Crash reports no longer litter the working directory

When `mutsu` died of a fatal signal (SIGSEGV, SIGABRT, ...) with
`MUTSU_CRASH_DIR` unset, its crash handler wrote `tmp/crash/<pid>.txt` under
the current directory, mode 0644 and truncating any existing file. That was
harmless inside this repository, where `tmp/` is gitignored, but the shipped
binary did the same in any directory a user ran it from, and the report
carries the full argv (so `-e` program text) and the cwd (#11219).

There is now no default directory at all: without `MUTSU_CRASH_DIR` the report
goes to stderr and nothing is written to disk. With `MUTSU_CRASH_DIR` set (CI
and the stress workflows export an absolute one) the report goes to a fresh
`<pid>.txt` in that directory — created 0700, the file 0600, opened with
`O_EXCL|O_NOFOLLOW` so an earlier report is never overwritten (a reused pid
gets `<pid>-1.txt`, ...) and a planted symlink is never followed — and stderr
gets one line naming the file.
