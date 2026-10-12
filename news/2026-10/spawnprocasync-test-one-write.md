# `nqp-spawnprocasync.t` no longer depends on pipe chunking

The `nqp::spawnprocasync` test checked that the child gets the `cwd` and
environment it was given. It ran `pwd; echo $FOO` and expected both lines in a
single `out` event. The shell writes them separately, though, so under
`prove -j` load the reader sometimes ran between the two writes and saw two
chunks, which failed the test (#12931). The child now prints both lines with
one `printf`, so the check no longer depends on how the reads are scheduled.
